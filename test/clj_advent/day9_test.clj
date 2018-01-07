(ns clj-advent.day9-test
  (:require [clojure.test :refer :all]))

(defn one-char [state char]
  (let [last (peek state)]
    (if (and (= char \!) (not= last :escape))
      {:state (conj state :escape)}
      (case last
        :escape {:state (pop state)}
        :garbage {:state (if (= char \>)
                           (pop state)
                           state)}
        :group (case char
                 \< {:state (conj state :garbage)}
                 \} {:state (pop state)
                     :cmd   :finish-group}
                 \{ {:state (conj state :group)
                     :cmd   :start-group}
                 {:state state})
        :start (case char
                 \{ {:state (conj state :group)
                     :cmd   :start-group}
                 \< {:state (conj state :garbage)}
                 {:state state})))))

(defn score [input]
  (reduce (fn [s c] (let [result (one-char (:state s) c)]
                      (-> s
                          (assoc :state (:state result))
                          (update :depth + (case (:cmd result)
                                             :start-group 1
                                             :finish-group -1
                                             0))
                          (update :sum + (if (= (:cmd result) :finish-group)
                                           (:depth s)
                                           0)))))
          {:state '(:start) :sum 0 :depth 0} input))

(deftest day9
  (testing "the examples"
    (is (= 1 (:sum (score "{}"))))
    (is (= 6 (:sum (score "{{{}}}"))))
    (is (= 5 (:sum (score "{{},{}}"))))
    (is (= 16 (:sum (score "{{{},{},{{}}}}"))))
    (is (= 1 (:sum (score "{<a>,<a>,<a>,<a>}"))))
    (is (= 9 (:sum (score "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))
    (is (= 9 (:sum (score "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))
    (is (= 3 (:sum (score "{{<a!>},{<a!>},{<a!>},{<ab>}}")))))

  (testing "the thing"
    (is (= 14212 (:sum (score (slurp "test-resources/day9.input")))))))