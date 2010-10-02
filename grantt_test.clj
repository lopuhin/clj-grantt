(ns csp.grantt-test
  (:use [clojure.test :only (deftest is are run-tests)]))

(use 'csp.grantt :reload)

(deftest time-intersected-test
  (are [intersection? start-a dur-a start-b dur-b]
       (= (if intersection? #{{:id "b" :real-duration dur-b}} #{})
          (set (time-intersected
                {:id "a" :duration dur-a} {:a-start start-a :b-start start-b}
                [{:id "a" :duration dur-a} {:id "b" :real-duration dur-b}])))
       true 0 10 1 5
       false 0 10 10 1
       false 10 1 0 10 
       true 3 6 5 2
       false 3 2 7 2))

(deftest build-n-conflicts-test
  (let [csp (grantt-csp (parse-input "src/csp/source.csv"))
        n-conflicts (:n-conflicts csp)]
    (are [variable value bindings n]
         (= n (n-conflicts variable value bindings))
         :A1-start 5 {:A2-start 2} 1
         :A1-start 5 {} 0
         :A1-start 10 {:A2-start 40 :A1-resource "D" :A2-resource "D"} 0
         :A1-start 0 {:R1-start 0 :A1-resource "D" :R1-resource "D"} 1
         )))

(deftest build-date-mapping-test
  (let [[hours date->hour hour->date]
        (build-date-mapping (parse-input "src/csp/source.csv"))]
    (is (= hours [0 408]))
    (are [date-str hour]
         (= (date->hour (parse-date date-str)) hour)
         "11/21/2009" 0 "11/22/2009" 0 ;; это выходные, вообще-то
         "11/23/2009" 0 "11/24/2009" 8
         "11/25/2009" 16 "11/30/2009" 32) ;; проверяем пропущенный выходной 11/26/2009
    (are [hour date-str]
         (= (hour->date hour) (parse-date date-str))
         0 "11/23/2009" 8 "11/24/2009")))

(deftest fixed-vars-test
  (is (= (set (fixed-vars [[:fn "fixrt" ["A" "B"]] [:fn "foo" ["c" "d"]]
                           [:rel "fixrt" ["e"]] [:fn "fixr" ["f"]]]
                          #{"fixrt" "fixr"} identity))
              #{"A" "B" "f"})))

(deftest build-domains-test
  (let [csp (grantt-csp (parse-input "src/csp/source.csv"))
        domains (:domains csp)]
    (are [variable domain]
         (= (sort (domains variable)) (sort domain))
         :A1-start (range (- 408 16))
         :A2-start (range (- 408 24))
         :A1-resource ["D" "T"]
         :S3-resource ["T"]
         :S3-start [224]
         ))
  (let [csp (grantt-csp (parse-input "src/csp/corrected.csv"))
        domains (:domains csp)]
    (are [variable domain] 
         (= (sort (domains variable)) (sort domain))
         :C2-start (range 48 (- 240 40))
         :A2-start [16]
         :C2-resource ["D" "T"]
         :A2-resource ["T"]
         )))

(deftest format-date-test
  (are [date] (= (format-date (parse-date date)) date)
       "11/21/2009" "1/3/2010"))

(deftest get-holidays-test
  (let [data (parse-input "src/csp/source.csv")]
    (is (= (set (map parse-date ["11/26/2009","1/1/2010"])) (get-holidays data))))) 

(deftest output-test
  (let [bindings
        {:C3-start 116, :Q2-start 361, :A1-resource "D", :C1-resource "T",
         :S3-start 224, :S3-resource "T", :R3-start 2, :R1-start 247,
         :Left-start 317, :Q2-resource "D", :C2-start 205, :A1-start 51,
         :C3-resource "D", :Q1-start 160, :Q1-resource "D", :R3-resource "D",
         :C2-resource "D", :A2-resource "T", :C1-start 90, :Left-resource "T",
         :R2-start 17, :R1-resource "D", :A2-start 124, :R2-resource "T"}
        data (parse-input "src/csp/source.csv")]
    (output-solution "/tmp/foo.csv" data bindings)))

(run-tests)
