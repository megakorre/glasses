(ns glasses.tree-test
  (:require [glasses.tree       :as tree]
            [glasses            :as lens]
            [glasses.traversals :as travers]
            [clojure.xml        :as xml]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(defn parse-xml-str [xml-text]
  (xml/parse
   (java.io.ByteArrayInputStream.
    (.getBytes xml-text))) )

(def data (parse-xml-str
           "<books>
             <book title=\"my little pony\">
               <author name=\"john\" />
             </book>
             <book title=\"the big star\">
               <author name=\"lars\" />
             </book>
            </books>"))

(deftest pre-tree-test
  (let [xml-nodes (tree/pre-tree [:content travers/mapped])]

    (is (= (lens/view data [xml-nodes :tag])
           [:books :book :author :book :author]))

    (let [books [xml-nodes (travers/shrink (fn [{t :tag}] (= t :book)))]
          title [:attrs :title]
          author [:content travers/mapped (travers/shrink (fn [{t :tag}] (= t :author)))]]

      (is (= (lens/view data [books {:title title
                                     :authors [author :attrs :name]}])
             [{:title "my little pony" :authors ["john"]}
              {:title "the big star"   :authors ["lars"]}]))

      (is (= (-> data
                 (lens/update [books title] string/capitalize)
                 (lens/view   [books title]))

             ["My little pony" "The big star"])))))
