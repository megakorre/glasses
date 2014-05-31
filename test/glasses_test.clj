(ns glasses-test
  (:require [glasses :as l]
            [clojure.test :refer :all]))

(deftest id-lens-test

  (testing "returns the item itself when viewed"
    (is (= (l/view :value l/id)
           :value)))

  (testing "it runs the function on whatever value its given"
    (is (= (l/update 1 l/id inc)
           2)))

  (testing "it replaces the value with whatever written to it"
    (is (= (l/write :x l/id :y)
           :y))))

(deftest assoc-lens-test
  (let [lens (l/assoc-lens "hej")]
    (testing "it returns the expected values"
      (are [input result] (= (l/view input lens) result)
           {"hej" :v}  :v
           {}          nil
           {"hej" nil} nil))

    (testing "it replaces the field like expected"
      (are [input new-val result]
        (= (l/write input lens new-val) result)
        {"hej" :v} :v2 {"hej" :v2}
        {}         :v2 {"hej" :v2}
        {"hej" :v} nil {}))))

(deftest keyword-lens
  (testing "keyword behaves as lenses"
    (is (= (l/view {:a 1} :a) 1))
    (is (= (l/update {:a 1} :a inc) {:a 2}))))

(deftest lens-composition
  (let [new-lens (l/comp-lens :a :b)]
    (testing "the result of composing 2 lenses is a lens to the
              bottom of the structure"
      (is (= (l/write {:a {:b 2}} new-lens 3)
             {:a {:b 3}}))

      (is (= (l/view {:a {:b 2}} new-lens) 2)))))


(deftest vector-lens-test
  (testing "a vector of lensables should behave as the composition
            of all its lenses"

    (testing "writing"
      (are [input lens new-val result]
        (= result (l/write input lens new-val))

        0                  []         1    1 ; id lens
        {:a 1}             [:a]       2    {:a 2}
        {:a {:b 2}}        [:a :b]    3    {:a {:b 3}}
        {:a {:b {:c 4}}}   [:a :b :c] 5    {:a {:b {:c 5}}}))

    (testing "viewing"
      (are [input lens result]
        (= result (l/view input lens))

        0                  []         0 ; id lens
        {:a 1}             [:a]       1
        {:a {:b 2}}        [:a :b]    2
        {:a {:b {:c 4}}}   [:a :b :c] 4))))

(deftest mapped-test
  (testing "it lets you write to all items of a seq"
    (are [input new-val output]
      (= (l/write input l/mapped new-val) output)

      []      2  []
      [1]     2  [2]
      [1 2]   2  [2 2]
      [1 2 3] 2  [2 2 2]))

  (testing "it lets you update all values of a seq"
    (are [input f output]
      (= (l/update input l/mapped f) output)

      []      inc  []
      [1]     inc  [2]
      [1 2]   inc  [2 3]
      [1 2 3] inc  [2 3 4]))

  (testing "it lets you view all values of a seq"
    (are [input output]
      (= (l/view input l/mapped) output)

      []      []
      [1]     [1]
      [1 2]   [1 2]
      [1 2 3] [1 2 3])))

(deftest traversal-composition
  (testing "composing a traversal with a lens"
    (are [input f output]
      (= output (l/update input [l/mapped :a] f))

      []              inc []
      [{:a 1}]        inc [{:a 2}]
      [{:a 1} {:a 2}] inc [{:a 2} {:a 3}]))

  (testing "composing a lens with a traversal"
    (are [input f output]
      (= output (l/update input [:a l/mapped] f))

      {:a []}      inc  {:a []}
      {:a [1]}     inc  {:a [2]}
      {:a [1 2 3]} inc  {:a [2 3 4]}))

  (testing "composing a lens traversal and a lens"
    (are [input f output]
      (= output (l/update input [:a l/mapped :b] f))

      {:a []}              inc {:a []}
      {:a [{:b 1}]}        inc {:a [{:b 2}]}
      {:a [{:b 1} {:b 2}]} inc {:a [{:b 2} {:b 3}]})))

(deftest extract-lens-test
  (testing "can extract a map of lenses"
    (is (= (l/view {:a 1 :b 2} (l/extract {:a :b :b :a}))
           {:a 2 :b 1})))

  (testing "can modify a extracted value"
    (is (= (l/update {:a 1 :b 2} (l/extract {:a :b :b :a})
                     assoc :a 3)
           {:a 1 :b 3}))))

(deftest flatten-lenses-test
  (testing "can flatten lenses to a single lens"
    (is (= (l/update {:a 1 :b 2} [(l/flatten-lenses [:a :b]) l/mapped] inc)
           {:a 2 :b 3}))

    (is (= (l/write {:a 1 :b 2} [(l/flatten-lenses [:a :b])] [5 4])
           {:a 5 :b 4}))))

(deftest map-behaves-like-extract-test
  (testing "can use a hash of lenses like an extract of that hash"
    (is (= (l/update {:a 1 :b 2}  [{:a :b :b :a} :a] inc)
           {:a 1 :b 3}))))

(deftest filtered-test
  (testing "can view a subset of a list"
    (is (= (l/view [1 2 3 4] (l/filtered odd?))
           [1 3])))

  (testing "can update a subset of a list"
    (is (= (l/update [1 2 3 4] (l/filtered odd?) inc)
           [2 2 4 4]))))

(deftest iso-lens-test
  (testing "can use two roundtrip functions to create a lens"
    (let [lens (l/iso-lens #(Integer/parseInt %)
                           #(.toString %))]
      (is (= (l/view "1" lens) 1))
      (is (= (l/update "1" lens inc) "2")))))

(deftest find-lens-test
  (testing "given a function returning a lens gives you a lens"
    (let [disposable (l/find-lens (fn [r] (if (:hungry r) :gold :food)))]
      (is (= (l/update {:hungry false :gold 2 :food 2} disposable dec)
             {:hungry false :gold 2 :food 1}))
      (is (= (l/update {:hungry true :gold 2 :food 2} disposable dec)
             {:hungry true :gold 1 :food 2})))))

(deftest mapped-vals-test
  (testing "given a map it can update all its vals"
    (is (= (l/update {:a 1 :b 2} l/mapped-vals inc)
           {:a 2 :b 3}))))

(deftest getter-setter-lens-test
  (testing "given a getter and a setter it gives you a lens"
    (let [lens (l/getter-setter-lens :a #(assoc %1 :a %2))]
      (is (= (l/update {:a 1} lens inc) {:a 2}))
      (is (= (l/view   {:a 1} lens)     1)))))
