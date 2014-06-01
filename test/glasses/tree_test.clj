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

(defn shrink-tag [tag-name]
  (travers/shrink (fn [{t :tag}] (= t tag-name))))

(def xml-nodes   (tree/pre-tree [:content travers/mapped]))

(def books       [xml-nodes (shrink-tag :book)])
(def book-title  [:attrs :title])
(def book-author [:content travers/mapped (shrink-tag :author)])

(deftest pre-tree-test
  (is (= (lens/view data [xml-nodes :tag])
         [:books :book :author :book :author]))

  (is (= (lens/view data [books {:title book-title
                                 :authors [book-author :attrs :name]}])
         [{:title "my little pony" :authors ["john"]}
          {:title "the big star"   :authors ["lars"]}]))

  (is (= (-> data
             (lens/update [books book-title] string/capitalize)
             (lens/view   [books book-title]))

         ["My little pony" "The big star"])))
