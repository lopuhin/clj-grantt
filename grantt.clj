(ns csp.grantt
  (:use [clojure.contrib.str-utils :only (re-split)]
        [clojure.contrib.duck-streams :only (spit)]
        [clojure.contrib.command-line :only (with-command-line)]
        [csp.min-conflicts :only (min-conflicts-restarts csp-s)]
        [utils.common :only (count-if)]
        [utils.csv :only (parse-csv write-csv)])
  (:import (java.text DateFormat ParseException)
           java.util.GregorianCalendar))

(declare parse-dates parse-tasks parse-constraint parse-first-with)

(defstruct grantt-s :dates :tasks :constraints :resources)

(defstruct task-s
  :id :title :duration :real-duration :done :remains :resource :start :end :comment)

(defn parse-input
  "Считываем файл с условиями в формате csv, и возвращаем словарь с
   датами :dates, задачами :tasks, ресурсами :resorces и ограничениями :constraints"
  [input-file]
  (let [parsers {"Dates" parse-dates
                 "Tasks" parse-tasks
                 "Constraints" (parse-first-with parse-constraint)
                 "Resources" (parse-first-with identity)}]
    (loop [lines (parse-csv (slurp input-file)) data {}]
      (if lines
        (let [section-name (re-find #"\w+" (ffirst lines))
              parser (parsers section-name)]
          (if parser
            (let [[data-lines rem-lines] (split-with #(not (every? empty? %)) lines)
                  key (keyword (.toLowerCase section-name))]
              (recur (next rem-lines)
                     (update-in data [key] concat (parser data-lines))))
            (recur (next lines) data)))
        data))))

(defn parse-date
  "Парсим дату в формате 11/21/2009, возвращая GregorianCalendar, или nil при неудаче"
  [date-str]
  (let [df (DateFormat/getDateInstance DateFormat/SHORT)]
    (try
     (doto (GregorianCalendar.)
       (.setTime (.parse df date-str)))
     (catch ParseException e))))

(defn parse-dates [lines]
  (let [[heading start end curr] (take 4 (first lines))]
    (map parse-date [start end curr])))

(defn parse-tasks [lines]
  (map #(let [t (apply struct task-s %)]
          (conj t {:start (parse-date (:start t))
                   :end (parse-date (:end t))
                   :duration (Integer/parseInt (:duration t))
                   :real-duration (if (empty? (:real-duration t)) nil
                                      (Integer/parseInt (:real-duration t)))}))
       (rest lines)))

(defn parse-constraint [constraint]
  (if-let [[_ function args] (re-find #"^([\w\d]+)\(([^)]+)\)$" constraint)]
    [:fn function (re-split #"\s*,\s*" args)]
    (if-let [[_ a rel b]
             (re-find #"^([\w\d]+)\s+(<<|>>|<\!|\!>)\s+([\w\d]+)$" constraint)]
      [:rel rel [a b]])))

(defn parse-first-with [function]
  (fn [lines] (filter identity (map (comp function first) (rest lines)))))

;; ==================================================
;; Компиляция условий - формирование из них csp-задачи
;; ==================================================

(defn start-var "Переменная, обозначающая начало выполнения задания"
  [task-id] (keyword (str task-id "-start")))

(defn start-var? "Обозначает ли эта переменная начало выполнения задания?"
  [variable] (.endsWith (name variable) "-start"))

(defn resource-var "Переменная, обозначающая исполнителя задания"
  [task-id] (keyword (str task-id "-resource")))

(defn resource-var? "Обозначает ли эта переменная исполнителя задания?"
  [variable] (.endsWith (name variable) "-resource"))

(defn task-duration
  "Продолжительность задания - или уточненная, или данная изначально"
  [task] (or (:real-duration task) (:duration task)))

(defn time-intersected
  "Задачи, которые пересекаются с данной по времени исполнения как полуинтервалы [----)"
  [task bindings tasks]
  (let [task-start-end (fn [t] (let [s (bindings (start-var (:id t)))]
                                 [s (+ s (task-duration t))]))
        in-int (fn [x s e] (and (> x s) (< x e))) ;; точка в интервале (---)
        in-hint (fn [x s e] (and (>= x s) (< x e))) ;; точка в полуинтервале [---)
        [s1 e1] (task-start-end task)]
    (filter (fn [t] (let [[s2 e2] (task-start-end t)]
                      (or (in-hint s1 s2 e2) (in-int e1 s2 e2)
                          (in-hint s2 s1 e1) (in-int e2 s1 e1))))
            (filter #(and (not= % task) (bindings (start-var (:id %)))) tasks))))

(defn compile-relation-constraint
  "По ограничению на отношения строим функцию, проверяющую выполнение этого отношения
   (то есть возвращающая true если ограничение выполняется)"
  [[_ kind [task1-id task2-id]] id->task]
  (let [t1-dur (task-duration (id->task task1-id))
        t2-dur (task-duration (id->task task2-id))
        t1-start-var (start-var task1-id)
        t2-start-var (start-var task2-id)
        fns
        ;; Конец первого раньше начала второго
        {"<<" (fn [bindings] (<= (+ (bindings t1-start-var) t1-dur)
                                (bindings t2-start-var)))
         ;; Конец первого раньше конца второго 
         "<!" (fn [bindings] (< (+ (bindings t1-start-var) t1-dur)
                                (+ (bindings t2-start-var) t2-dur)))}
        fns (conj fns {">>" #(not ((fns "<<") %))
                       "!>" #(not ((fns "<!") %))})]
    (fn [bindings] (if (every? bindings [t1-start-var t2-start-var])
                     ((fns kind) bindings) true))))

(defn build-var->constraints
  "Словарь: для каждой переменной - вектор функций-ограничений, которые ее связывают"
  [data variables id->task]
  (reduce
   (fn [cs-map constraint]
     (let [constraint-fn (compile-relation-constraint constraint id->task)]
       (reduce #(update-in %1 [(start-var %2)] conj constraint-fn) cs-map
               (nth constraint 2))))
   (zipmap (filter start-var? variables) (repeat []))
   (:constraints data)))

(defn build-n-conflicts
  "Функция, возвращающая число противоречий для данного присваивания"
  [data variables id->task var->task]
  (let [var->constraints (build-var->constraints data variables id->task)]
    (fn [variable value bindings]
      (let [bindings (assoc bindings variable value)
            task (var->task variable)]
        (+ ;; Проверяем, нет ли у одного испольнителя нескольких задач в одно время
         (count-if (fn [t](let [[r1 r2] (map (comp bindings resource-var :id) [t task])]
                            (and r1 r2 (= r1 r2))))
                   (time-intersected task bindings (:tasks data)))
         ;; а также специальные условия с этим заданием
         (count-if #(not (% bindings)) (var->constraints variable)))))))

(defn get-holidays
  "Возвращаем множество праздников из секции :constraints" [data]
  (set (mapcat #(map parse-date %)
               (for [[kind fn args] (:constraints data)
                     :when (= [kind fn] [:fn "hl"])] args))))

(def n-work-hours 8)
(def work-days #{2 3 4 5 6})

(defn build-date-mapping [data]
  "Время дискретно, шаг - один час. Каждый день, кроме выходных, составляет 8 часов.
   Здесь строится отображение дат в условное время, в котором рабочие часы 
   идут подряд (и обратное отображение). Возвращаем область допустимых значений для
   времени и две функции - одна по дате возвращает число (время) - начало текущих суток,
   а другая - дату по числу (времени)."
  (let [[start-date end-date current-date] (:dates data)
        holiday? (get-holidays data)
        next-date (fn [d] (doto (.clone d)
                            (.set GregorianCalendar/DAY_OF_YEAR
                                  (+ (.get d GregorianCalendar/DAY_OF_YEAR) 1))))
        work-dates (filter #(and (work-days (.get % GregorianCalendar/DAY_OF_WEEK))
                                 (not (holiday? %)))
                           (take-while #(not (.after % end-date))
                                       (iterate next-date start-date)))
        n->date (zipmap (iterate inc 0) work-dates)
        date->n (zipmap work-dates (iterate inc 0))]
    [[(* n-work-hours (date->n current-date)) (* n-work-hours (count work-dates))]
     (fn [date] ;; ищем первый рабочий день
       (* (first (drop-while not (map date->n (iterate next-date date))))
          n-work-hours))
     (fn [hour] (n->date (int (/ hour n-work-hours))))]))

(defn fixed-vars
  "Переменные, фиксированные заданными функциями" [constraints kinds var-fn]
  (mapcat #(map var-fn %)
          (for [c constraints :when (and (= (first c) :fn) (kinds (second c)))]
            (nth c 2))))

(defn build-domains
  "Функция, возвращающая диапазон значений для каждой переменной" [data var->task]
  (let [[[start-hour end-hour] date->hour hour->date] (build-date-mapping data)
        current (date->hour (last (:dates data)))
        task-start (fn [t] (if-let [s (:start t)] (date->hour s)))
        past-tasks (filter #(if-let [s (task-start %)] (<= s current)) (:tasks data))
        ;; Переменные, фиксированные в по специальным условиям, или те, что в прошлом
        ;; Для начал заданий из прошлого задаем диапазон длиной в рабочий день,
        ;; ведь мы не знаем, во сколько именно они начались, и у нас может выполниться
        ;; несколько заданий за один день (см случай fixed-starts-vars)
        fixed-resources-vars
        (set (concat (fixed-vars (:constraints data) #{"fixrt" "fixr"} resource-var)
                     (map (comp resource-var :id) past-tasks)))
        fixed-starts-vars
        (set (concat (fixed-vars (:constraints data) #{"fixrt" "fixt"} start-var)
                     (map (comp start-var :id) past-tasks)))]
    (fn [v] (cond (fixed-resources-vars v) [(:resource (var->task v))]
                  (fixed-starts-vars v) (let [s (task-start (var->task v))]
                                          (range s (+ s n-work-hours)))
                  (start-var? v) (range start-hour
                                        (- end-hour (task-duration (var->task v))))
                  (resource-var? v) (:resources data)))))

(defn grantt-csp
  "Строим csp-задачу по описанию ограничений и условий на процессы" [data]
  (let [;; Переменные - для каждой задачи начало ее выполненния, и исполнитель
        variables (mapcat #(map % (map :id (:tasks data))) [start-var resource-var])
        var->task (zipmap variables (concat (:tasks data) (:tasks data)))
        id->task (zipmap (map :id (:tasks data)) (:tasks data))
        domains (build-domains data var->task)
        n-conflicts (build-n-conflicts data variables id->task var->task)]
    (struct csp-s variables domains n-conflicts)))

(defn format-date
  "Форматируем дату в виде mm/dd/yyyy" [date]
  (format "%s/%s/%s" (inc (.get date GregorianCalendar/MONTH))
          (.get date GregorianCalendar/DAY_OF_MONTH)
          (.get date GregorianCalendar/YEAR)))

(defn format-solution [data bindings]
  (let [[_ _ hour->date] (build-date-mapping data)]
    (write-csv
     (map ;; Дополняем "" до 10 и приводим все к строке
      (fn [fields] (concat (map str fields) (repeat (- 10 (count fields)) "")))
      (concat
       [[] (concat ["Dates:"] (map format-date (:dates data)))
        [] ["Tasks:", "","нач. оценка","уточненная оценка","сделано","осталось",
            "исполнитель","время начала","время окончания","комментарий"]]
       (for [t (:tasks data)]
         [(:id t) (:title t) (:duration t) (:real-duration t)
          (:done t) (:remains t)
          (bindings (resource-var t))
          (format-date (hour->date (bindings (start-var (:id t)))))
          (format-date (hour->date (+ (bindings (start-var (:id t)))
                                      (task-duration t))))
          (:comment t)])
       [[] ["Constraints:"]]
       (for [c (:constraints data)]
         (if (= (first c) :rel)
           (let [[_ op [arg1 arg2]] c] [(str arg1 " " op " " arg2)])
           (let [[_ fn args] c]
             [(format "%s(%s)" fn (apply str (interpose ", " args)))])))
       [[] ["Resources:"]] (map vec (:resources data))
       [[] ["Plan Format:", "1.0"]])))))
    
(defn output-solution
  "Вывод получившегося решения в формате csv в файл или на экран"
  [out-file data bindings]
  (let [output (format-solution data bindings)]
    (if out-file (spit out-file output)
        (println output))))
  
(with-command-line *command-line-args*
  "Планирование процессов Гангта"
  [[n-restarts "Число перезапусков min-conflicts" 10]
   [n-iters "Число итераций min-conflicts" 100]
   filenames]
  (let [[in-file out-file] (drop-while #(.endsWith *file* %) filenames)
        data (parse-input in-file)
        solution (min-conflicts-restarts (grantt-csp data) n-restarts n-iters)]
    (if solution
      (do (prn solution)
          (output-solution out-file data solution))
      (prn "Не получилось!"))))
