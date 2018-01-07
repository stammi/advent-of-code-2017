(ns clj-advent.day9-test
  (:require [clojure.test :refer :all]))

(defn one-char [state char]
  (let [last (peek state)]
    (if (and (= char \!) (not= last :escape))
      {:state (conj state :escape)}
      (case last
        :escape {:state (pop state)}
        :garbage (if (= char \>)
                   {:state (pop state)}
                   {:state state :cmd :count-garbage})
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
                          (update :garbage-count + (if (= (:cmd result) :count-garbage)
                                                     1
                                                     0))
                          (update :sum + (if (= (:cmd result) :finish-group)
                                           (:depth s)
                                           0)))))
          {:state '(:start) :sum 0 :depth 0 :garbage-count 0} input))

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


  (testing "garbage examples"
    (is (= (:garbage-count (score "<>")) 0))
    (is (= (:garbage-count (score "<random characters>")) 17))
    (is (= (:garbage-count (score "<<<<>")) 3))
    (is (= (:garbage-count (score "<{!>}")) 2))
    (is (= (:garbage-count (score "<!!>")) 0))
    (is (= (:garbage-count (score "<!!!>>")) 0))
    (is (= (:garbage-count (score "<{o\"i!a,<{i<a>")) 10)))

  (testing "the thing"
    (let [result (score (slurp "test-resources/day9.input"))]
      (is (= 14212 (:sum result)))
      (is (= 6569 (:garbage-count result))))))


