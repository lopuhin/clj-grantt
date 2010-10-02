(ns csp.min-conflicts
  (:use [clojure.test :only (deftest is run-tests)]
        [clojure.contrib.str-utils :only (re-split)]
        [utils.common :only (build-map count-if)]))

(defstruct
    #^{:doc "Задача на удовлетворение ограничений (CSP)
    :vars - переменные задачи, :domains - их области значений, а
    :n-conflicts - функция, которая по [variable value assignment] возвращает
    число конфликтов с условиями задачи при таком присваивании (variable <- value)"}
  csp-s :vars :domains :n-conflicts)

(declare min-conflicts min-conflicts-value conflicted-vars
         random-choice argmin-random-tie initial-binding)
 
(defn min-conflicts
  "Решаем CSP задачу методом минимальных конфликтов: на каждом шаге сучайно выбираем
   переменную, которая противоречит условиям задачи, и присваиваем ей значение, которое 
   конфликтует с наименьшим числом условий."
  [csp max-steps]
  (loop [current (initial-binding csp) n max-steps]
    (let [conflicted (conflicted-vars csp current)]
      (println n conflicted)
      (if (empty? conflicted)
        current
        (if (pos? n)
          (recur (let [v (random-choice conflicted)]
                   (assoc current v (min-conflicts-value csp v current))) (dec n))
          nil)))))

(defn min-conflicts-restarts
  "Используем алгоритм min-conflicts, перезапуская несколько раз при неудаче. Кроме проблемы
   нужно задать число итераций алгоритма и число перезапусков"
  [csp n-restarts n-iters]
  (first (drop-while not (map (fn [_] (min-conflicts csp n-iters))
                              (repeat n-restarts nil)))))

(defn initial-binding
  "Присваиваем каждой переменной значение, минимизируещее число конфликтов"
  [csp] (reduce (fn [current v]
                  (assoc current v (min-conflicts-value csp v current))) {}
                (:vars csp)))

(defn min-conflicts-value
  "Значение переменной v, которое приводит к наименьшему числу конфликтов"
  [csp v current]
  (argmin-random-tie ((:domains csp) v)
                     (fn [val] ((:n-conflicts csp) v val current))))

(defn random-choice
  "Случайный элемент последовательности coll"
  [coll] (let [vect (vec coll)] (vect (rand-int (count vect)))))

(defn argmin-random-tie
  "Значение из coll, для которой f возвращает наименьшее значение. При нескольких равных
   значениях выбираем из них случайное"
  [coll f]
  (loop [coll coll, best-score (f (first coll)), best (first coll), n-best 1]
    (if coll
      (let [cur (first coll), cur-score (f cur)]
        (cond
          (< cur-score best-score) (recur (next coll) cur-score cur 1)
          (= cur-score best-score) (recur (next coll) best-score
                                          (if (= (rand-int (inc n-best)) 0) cur best)
                                          (inc n-best))
          :default (recur (next coll) best-score best n-best)))
      best)))

(defn conflicted-vars
  "Переменные CSP, которые противоречат условиям задачи"
  [csp assignment]
  (filter (fn [v] (pos? ((:n-conflicts csp) v (assignment v) assignment)))
          (:vars csp)))

(defn make-binary-n-conflicts
  "Функия n-conflicts для случая бинарных ограничений (каждое ограничение связывает две
  переменные). Функция constraints возвращает true когда переменные принимают допустимые
  значения. А neighbours - словарь, возвращающий переменные, которые связаны с данной
  каким-либо условием."
  [neighbours constraints]
  (fn [variable value assignment]
    (count-if (fn [v] (and (assignment v)
                           (not (constraints variable value v (assignment v)))))
              (neighbours variable))))

;; =====================================================================
;; Проверяем корректность работы алгоритма на проблеме раскраски карты США
;; =====================================================================

(defn parse-neighbours
  "Строим словарь из строки вида 'SA: WA NT Q NSW V; NT: WA Q; NSW: Q V; T: '"
  [neighbours]
  (build-map
   (for [[key vals] (map #(re-split #":" %) (re-split #";" neighbours))]
     [(keyword (.trim key))
      (vec (for [v (re-split #"\s" (.trim vals)) :when (seq v)] (keyword v)))])))

(defn map-coloring-csp
  "Проблема по раскраске карты. Передаются строка с цветами, в которые мы хотим ее
   покрасить, и строка, которая задает области и их соседей"
  [colors neighbours]
  (let [neighbours (parse-neighbours neighbours)
        colors (vec (map (comp keyword str) colors))]
    (struct csp-s (vec (keys neighbours)) (fn [v] colors)
            (make-binary-n-conflicts neighbours (fn [A a B b] (not= a b))))))

(deftest usa-coloring-csp
  (is (min-conflicts-restarts (map-coloring-csp "rgby"
       "WA: OR ID; OR: ID NV CA; CA: NV AZ; NV: ID UT AZ; ID: MT WY UT;
        UT: WY CO AZ; MT: ND SD WY; WY: SD NE CO; CO: NE KA OK NM; NM: OK TX;
        ND: MN SD; SD: MN IA NE; NE: IA MO KA; KA: MO OK; OK: MO AR TX;
        TX: AR LA; MN: WI IA; IA: WI IL MO; MO: IL KY TN AR; AR: MS TN LA;
        LA: MS; WI: MI IL; IL: IN; IN: KY; MS: TN AL; AL: TN GA FL; MI: OH;
        OH: PA WV KY; KY: WV VA TN; TN: VA NC GA; GA: NC SC FL;
        PA: NY NJ DE MD WV; WV: MD VA; VA: MD DC NC; NC: SC; NY: VT MA CA NJ;
        NJ: DE; DE: MD; MD: DC; VT: NH MA; MA: NH RI CT; CT: RI; ME: NH;
        HI: ; AK: ") 10 100)))

;;(run-tests)
