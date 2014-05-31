(ns glasses.traversals
  (:require [glasses :refer :all]))

(def mapped
  "traversal[a,b]"
  (traversal
   (fn [root]
     [root (fn [f] (map f root))])))

(defn- pred-map [pred? f collection]
  (for [item collection]
    (if (pred? item) (f item) item)))

(defn filtered
  "(b -> bool) -> traversal[a,b]"
  [pred?]
  (traversal
   (fn [root]
     [(filter pred? root)
      (fn [f] (pred-map pred? f root))])))

(def mapped-vals
  (find-lens
   (fn [hash-map]
     (comp-lenses [(flatten-lenses (map assoc-lens (keys hash-map)))
                   mapped]))))
