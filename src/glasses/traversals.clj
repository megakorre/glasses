(ns glasses.traversals
  (:refer-clojure :exclude [concat])
  (:require
   [clojure.core :as core]
   [glasses :refer :all]
   [glasses.protocols :refer [traversal? ->lens]]))

(def mapped
  "traversal[a,b]"
  (traversal
   (fn [root]
     [root (fn [f] (map f root))])
   (fn [] "mapped")))

(defn- pred-map [pred? f collection]
  (for [item collection]
    (if (pred? item) (f item) item)))

(defn filtered
  "(b -> bool) -> traversal[a,b]"
  [pred?]
  (traversal
   (fn [root]
     [(filter pred? root)
      (fn [f] (pred-map pred? f root))])
   (fn [] "filtered")))

(def mapped-vals
  (find-lens
   (fn [hash-map]
     (comp-lenses [(flatten-lenses (map assoc-lens (keys hash-map)))
                   mapped]))
   (fn [] "mapped-vals")))

(defn concat* [lens-a lens-b]
  (traversal
   (fn [root]
     [(core/concat
       (if (traversal? (->lens lens-a))
         (view root lens-a)
         [(view root lens-a)])
       (if (traversal? (->lens lens-b))
         (view root lens-b)
         [(view root lens-b)]))
      (fn [f]
        (-> root
            (update lens-a f)
            (update lens-b f)))])
   (fn [] (str "concat[" (str lens-a) " " (str lens-b) "]"))))

(defn concat [lenses]
  (reduce concat* ignore lenses))

(defn shrink [pred]
  (traversal
   (fn [item]
     (let [match? (pred item)]
       [(if match? [item] [])
        (fn [f]
          (if match? (f item) item))]))
   (fn [] "shrink")))
