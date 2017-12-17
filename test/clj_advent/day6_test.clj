(ns clj-advent.day6-test
  (:require [clojure.test :refer :all]))

(def input {:stepno 0
            :current [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5]})

(defn one-step [{:keys [stepno current]}]
  (let [vals-with-idx (map-indexed (fn [idx val] [val idx]) current)
        [max-val max-idx] (reduce (fn [[aggval aggidx] [val idx]]
                                    (if (>= aggval val)
                                      [aggval aggidx]
                                      [val idx]))
                                  [-1 -1] vals-with-idx)]
    (let [intermediate (assoc current max-idx 0)]
      (loop [result intermediate
             cursor (mod (inc max-idx) (count result))
             v max-val]
        (if (= 0 v)
          {:stepno  (inc stepno)
           :current result}
          (recur (update result cursor inc)
                 (mod (inc cursor) (count result))
                 (dec v)))))))

(defn all-steps [in]
  (loop [history []
         i in]
    (let [step (one-step i)]
      (let [previous (first (filter #(= (:current step) (:current %)) history))]
        (if previous
          (assoc step :loopsize (- (:stepno step) (:stepno previous)) )
          (recur (conj history step)
                 step))))))

(deftest redistributing-memory
  (testing "the examples"
    (let [step1 (one-step {:stepno 0 :current [0 2 7 0]})]
      (is (= 1 (:stepno step1)))
      (is (= [2 4 1 2] (:current step1)))
      (let [step2 (one-step step1)]
        (is (= [3 1 2 3] (:current step2)))
        (let [step3 (one-step step2)]
          (is (= [0 2 3 4] (:current step3)))
          (let [step4 (one-step step3)]
            (is (= [1 3 4 1] (:current step4)))
            (let [step5 (one-step step4)]
              (is (= [2 4 1 2] (:current step5)))
              (is (= 5 (:stepno step5))))))))
    (let [after-all (all-steps {:stepno 0 :current [0 2 7 0]})]
      (is (= 5 (:stepno after-all)))
      (is (= 4 (:loopsize after-all)))))

  (testing "the thing"
    (let [result (all-steps input)]
      (is (= 12841 (:stepno result)))
      (is (= 8038 (:loopsize result))))))