(ns glasses.traversals-test
  (:require [glasses.traversals :as t]
            [glasses            :as l]
            [clojure.test       :refer :all]))

(deftest mapped-test
  (testing "it lets you write to all items of a seq"
    (are [input new-val output]
      (= (l/write input t/mapped new-val) output)

      []      2  []
      [1]     2  [2]
      [1 2]   2  [2 2]
      [1 2 3] 2  [2 2 2]))

  (testing "it lets you update all values of a seq"
    (are [input f output]
      (= (l/update input t/mapped f) output)

      []      inc  []
      [1]     inc  [2]
      [1 2]   inc  [2 3]
      [1 2 3] inc  [2 3 4]))

  (testing "it lets you view all values of a seq"
    (are [input output]
      (= (l/view input t/mapped) output)

      []      []
      [1]     [1]
      [1 2]   [1 2]
      [1 2 3] [1 2 3])))

(deftest traversal-composition
  (testing "composing a traversal with a lens"
    (are [input f output]
      (= output (l/update input [t/mapped :a] f))

      []              inc []
      [{:a 1}]        inc [{:a 2}]
      [{:a 1} {:a 2}] inc [{:a 2} {:a 3}]))

  (testing "composing a lens with a traversal"
    (are [input f output]
      (= output (l/update input [:a t/mapped] f))

      {:a []}      inc  {:a []}
      {:a [1]}     inc  {:a [2]}
      {:a [1 2 3]} inc  {:a [2 3 4]}))

  (testing "composing a lens traversal and a lens"
    (are [input f output]
      (= output (l/update input [:a t/mapped :b] f))

      {:a []}              inc {:a []}
      {:a [{:b 1}]}        inc {:a [{:b 2}]}
      {:a [{:b 1} {:b 2}]} inc {:a [{:b 2} {:b 3}]})

    (are [input f output]
      (= output (l/update input [:a t/mapped :b t/mapped :c] f))

      {:a []}                     inc {:a []}
      {:a [{:b []}]}              inc {:a [{:b []}]}
      {:a [{:b [{:c 1}]}]}        inc {:a [{:b [{:c 2}]}]}
      {:a [{:b [{:c 1} {:c 2}]}]} inc {:a [{:b [{:c 2} {:c 3}]}]})))

(deftest filtered-test
  (testing "can view a subset of a list"
    (is (= (l/view [1 2 3 4] (t/filtered odd?))
           [1 3])))

  (testing "can update a subset of a list"
    (is (= (l/update [1 2 3 4] (t/filtered odd?) inc)
           [2 2 4 4]))))

(deftest mapped-vals-test
  (testing "given a map it can update all its vals"
    (is (= (l/update {:a 1 :b 2} t/mapped-vals inc)
           {:a 2 :b 3}))))

(deftest concat-tests
  (is (= (l/update 1 (t/concat [l/id l/id]) inc) 3))

  (is (= (l/view [1 2 3 4] (t/concat [(t/filtered odd?)
                                      (t/filtered even?)]))
         [1 3 2 4]))

  (is (= (l/update [1 2 3 4] (t/concat [(t/filtered odd?)
                                        (t/filtered even?)])
                   identity)
         [1 2 3 4])))

(deftest shrink-test
  (is (= (l/view [1 2 3 4] [t/mapped (t/shrink odd?)])
         [1 3]))

  (is (= (l/update [1 2 3 4] [t/mapped (t/shrink odd?)] inc)
         [2 2 4 4])))
