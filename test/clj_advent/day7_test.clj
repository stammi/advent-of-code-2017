(ns clj-advent.day7-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example ["pbga (66)"
              "xhth (57)"
              "ebii (61)"
              "havc (66)"
              "ktlj (57)"
              "fwft (72) -> ktlj, cntj, xhth"
              "qoyq (66)"
              "padx (45) -> pbga, havc, qoyq"
              "tknk (41) -> ugml, padx, fwft"
              "jptl (61)"
              "ugml (68) -> gyxo, ebii, jptl"
              "gyxo (61)"
              "cntj (57)"])

(defn parse-line [line]
  (let [[name weight arrow & children] (str/split line #"\h")
        base        {:name   name
         :weight (-> weight
                     (str/replace "(" "")
                     (str/replace ")" "")
                     Integer/parseInt)}]
    (if arrow
      (assoc base :children
                  (into [] (map #(str/replace % "," "") children)))
      base)))

(deftest parsing-lines
  (testing "some examples"
    (is (= {:name   "havc"
            :weight 66} (parse-line "havc (66)")))
    (is (= {:name "fwft"
            :weight 72
            :children ["ktlj" "cntj" "xhth"]}
           (parse-line "fwft (72) -> ktlj, cntj, xhth")))))

(defn find-bottom [example]
  (let [nodes (map parse-line example)
        all-names (into #{} (map :name nodes))
        all-children (into #{} (flatten (map :children nodes)))
        diff (set/difference all-names all-children)]
    (first diff)))

(deftest finding-the-bottom
  (testing "the examples"
    (is (= "tknk" (find-bottom example))))

  (testing "the thing"
    (is (= "aapssr"
           (-> (slurp "test-resources/day7.input")
               (str/split  #"\n")
               (find-bottom))))))
