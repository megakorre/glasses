(ns glasses-test
  (:require [glasses :as l]
            [glasses.traversals :as t]
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
  (let [new-lens (l/comp-lenses [:a :b])]
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
    (is (= (l/update {:a 1 :b 2} [(l/flatten-lenses [:a :b]) t/mapped] inc)
           {:a 2 :b 3}))

    (is (= (l/write {:a 1 :b 2} [(l/flatten-lenses [:a :b])] [5 4])
           {:a 5 :b 4}))))

(deftest map-behaves-like-extract-test
  (testing "can use a hash of lenses like an extract of that hash"
    (is (= (l/update {:a 1 :b 2}  [{:a :b :b :a} :a] inc)
           {:a 1 :b 3}))))

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

(deftest getter-setter-lens-test
  (testing "given a getter and a setter it gives you a lens"
    (let [lens (l/getter-setter-lens :a #(assoc %1 :a %2))]
      (is (= (l/update {:a 1} lens inc) {:a 2}))
      (is (= (l/view   {:a 1} lens)     1)))))
