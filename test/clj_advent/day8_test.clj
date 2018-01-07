(ns clj-advent.day8-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn read-instruction [instruction]
  (let [[action condition] (str/split instruction #" if ")
        [action-reg action-op action-val] (str/split action #" ")
        [cond-reg cond-op cond-val] (str/split condition #" ")
        cond-op-fn (case cond-op
                         "<" <
                         ">" >
                         ">=" >=
                         "<=" <=
                         "==" =
                         "!=" not=)
        action-op-fn (case action-op
                       "inc" +
                       "dec" -)]
    (fn [registers]
      (if (cond-op-fn (get registers cond-reg 0) (Integer/parseInt cond-val))
        (update registers action-reg #(action-op-fn (or % 0) (Integer/parseInt action-val)))
        registers))))

(defn apply-instructions [registers instructions]
  (let [ops (map read-instruction instructions)
        starter {:registers registers
              :max Integer/MIN_VALUE}]
    (reduce (fn [agg f]
              (let [new-regs (f (:registers agg))
                    max-reg (if (empty? new-regs) Integer/MIN_VALUE (reduce max (vals new-regs)))]
                {:registers new-regs
                 :max (max (:max agg) max-reg)})) starter ops)))

(deftest processing-instructions
  (testing "teh example"
    (let [instructions ["b inc 5 if a > 1"
                        "a inc 1 if b < 5"
                        "c dec -10 if a >= 1"
                        "c inc -20 if c == 10"]]
      (is (= {"a" 1
              "c" -10}
             (:registers (apply-instructions {} instructions))))))

  (testing "the thing"
    (let [instructions (-> (slurp "test-resources/day8.input")
                           (str/split #"\n"))]
      (is (= 5752 (reduce max (vals (:registers (apply-instructions {} instructions))))))
      (is (= 6366 (:max (apply-instructions {} instructions)))))))