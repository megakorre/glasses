(ns glasses.tree
  (:require [glasses            :as lens]
            [glasses.traversals :as travers]
            [clojure.walk       :as walk]))

(defn view-tree [traversal node]
  (cons node (mapcat (partial view-tree traversal)
                     (lens/view node traversal))))

(defn pre-replace-tree
  [traversal f node]
  (lens/update
   (f node)
   traversal
   (partial pre-replace-tree traversal f)))

(defn pre-tree [traversal]
  (lens/traversal
   (fn [node]
     [(view-tree traversal node)
      (fn [f] (pre-replace-tree traversal f node))])))

(defn post-replace-tree
  [traversal f node]
  (f
   (lens/update
    traversal
    (partial pre-replace-tree traversal f))))

(defn post-tree [traversal]
  (lens/traversal
   (fn [node]
     [(view-tree traversal node)
      (fn [f] (post-replace-tree traversal f node))])))
