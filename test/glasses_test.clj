(ns glasses-test
  (:require [glasses :as glasses]
            [clojure.test :refer :all]))

(deftest id-lens-test

  (testing "returns the item itself when viewed"
    (is (= (glasses/view :value glasses/id)
           :value)))

  (testing "it runs the function on whatever value its given"
    (is (= (glasses/update 1 glasses/id inc)
           2)))

  (testing "it replaces the value with whatever written to it"
    (is (= (glasses/write :x glasses/id :y)
           :y))))

(deftest assoc-lens-test
  (let [lens (glasses/assoc-lens "hej")]
    (testing "it returns the expected values"
      (are [input result] (= (glasses/view input lens) result)
           {"hej" :v}  :v
           {}          nil
           {"hej" nil} nil))

    (testing "it replaces the field like expected"
      (are [input new-val result]
        (= (glasses/write input lens new-val) result)
        {"hej" :v} :v2 {"hej" :v2}
        {}         :v2 {"hej" :v2}
        {"hej" :v} nil {}))))

(deftest keyword-lens
  (testing "keyword behaves as lenses"
    (is (= (glasses/view {:a 1} :a) 1))
    (is (= (glasses/update {:a 1} :a inc) {:a 2}))))

(deftest lens-composition
  (let [new-lens (glasses/comp-lens :a :b)]
    (testing "the result of composing 2 lenses is a lens to the
              bottom of the structure"
      (is (= (glasses/write {:a {:b 2}} new-lens 3)
             {:a {:b 3}}))

      (is (= (glasses/view {:a {:b 2}} new-lens) 2)))))


(deftest vector-lens-test
  (testing "a vector of lensables should behave as the composition
            of all its lenses"

    (testing "writing"
      (are [input lens new-val result]
        (= result (glasses/write input lens new-val))

        0                  []         1    1 ; id lens
        {:a 1}             [:a]       2    {:a 2}
        {:a {:b 2}}        [:a :b]    3    {:a {:b 3}}
        {:a {:b {:c 4}}}   [:a :b :c] 5    {:a {:b {:c 5}}}))

    (testing "viewing"
      (are [input lens result]
        (= result (glasses/view input lens))

        0                  []         0 ; id lens
        {:a 1}             [:a]       1
        {:a {:b 2}}        [:a :b]    2
        {:a {:b {:c 4}}}   [:a :b :c] 4))))

(deftest mapped-test
  (testing "it lets you write to all items of a seq"
    (are [input new-val output]
      (= (glasses/write input glasses/mapped new-val) output)

      []      2  []
      [1]     2  [2]
      [1 2]   2  [2 2]
      [1 2 3] 2  [2 2 2]))

  (testing "it lets you update all values of a seq"
    (are [input f output]
      (= (glasses/update input glasses/mapped f) output)

      []      inc  []
      [1]     inc  [2]
      [1 2]   inc  [2 3]
      [1 2 3] inc  [2 3 4]))

  (testing "it lets you view all values of a seq"
    (are [input output]
      (= (glasses/view input glasses/mapped) output)

      []      []
      [1]     [1]
      [1 2]   [1 2]
      [1 2 3] [1 2 3])))

(deftest traversal-composition
  (testing "composing a traversal with a lens"
    (are [input f output]
      (= output (glasses/update input [glasses/mapped :a] f))

      []              inc []
      [{:a 1}]        inc [{:a 2}]
      [{:a 1} {:a 2}] inc [{:a 2} {:a 3}]))

  (testing "composing a lens with a traversal"
    (are [input f output]
      (= output (glasses/update input [:a glasses/mapped] f))

      {:a []}      inc  {:a []}
      {:a [1]}     inc  {:a [2]}
      {:a [1 2 3]} inc  {:a [2 3 4]}))

  (testing "composing a lens traversal and a lens"
    (are [input f output]
      (= output (glasses/update input [:a glasses/mapped :b] f))

      {:a []}              inc {:a []}
      {:a [{:b 1}]}        inc {:a [{:b 2}]}
      {:a [{:b 1} {:b 2}]} inc {:a [{:b 2} {:b 3}]})))

(deftest extract-lens-test
  (testing "can extract a map of lenses"
    (is (= (glasses/view {:a 1 :b 2} (glasses/extract {:a :b
                                                       :b :a}))
           {:a 2 :b 1})))

  (testing "can modify a extracted value"
    (is (= (glasses/update {:a 1 :b 2}  (glasses/extract {:a :b
                                                          :b :a})
                           assoc :a 3)
           {:a 1 :b 3}))))

(deftest map-behaves-like-extract-test
  (testing "can use a hash of lenses like an extract of that hash"
    (is (= (glasses/update {:a 1 :b 2}  [{:a :b :b :a} :a] inc)
           {:a 1 :b 3}))))

(deftest filtered-test
  (testing "can view a subset of a list"
    (is (= (glasses/view [1 2 3 4] (glasses/filtered odd?))
           [1 3])))

  (testing "can update a subset of a list"
    (is (= (glasses/update [1 2 3 4] (glasses/filtered odd?) inc)
           [2 2 4 4]))))

(deftest iso-lens-test
  (testing "can use two roundtrip functions to create a lens"
    (let [lens (glasses/iso-lens #(Integer/parseInt %)
                                 #(.toString %))]
      (is (= (glasses/view "1" lens) 1))
      (is (= (glasses/update "1" lens inc) "2")))))

(deftest find-lens-test
  (testing "given a function returning a lens gives you a lens"
    (let [disposable (glasses/find-lens (fn [r] (if (:hungry r) :gold :food)))]
      (is (= (glasses/update {:hungry false :gold 2 :food 2} disposable dec)
             {:hungry false :gold 2 :food 1}))
      (is (= (glasses/update {:hungry true :gold 2 :food 2} disposable dec)
             {:hungry true :gold 1 :food 2})))))

(deftest getter-setter-lens-test
  (testing "given a getter and a setter it gives you a lens"
    (let [lens (glasses/getter-setter-lens :a #(assoc %1 :a %2))]
      (is (= (glasses/update {:a 1} lens inc) {:a 2}))
      (is (= (glasses/view   {:a 1} lens)     1)))))
