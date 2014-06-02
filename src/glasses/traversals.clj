(ns glasses.traversals
  (:refer-clojure :exclude [concat])
  (:require
   [clojure.core :as core]
   [glasses :as lens]
   [glasses.protocols :as proto]))

(defn ->traversal
  "coerce a lens or a traversal into a traversal
   if given a traversal its a noop
   if given a lens it returns a traversal of the 1 item the lens points to"
  [lens]
  (if (proto/traversal? lens)
    lens
    (lens/traversal
     (fn [item]
       [[item] (fn [f] (lens/update item lens f))])
     (fn [] (str "traversal[" (str lens) "]")))))

(def mapped
  "a traversal over a seq of items

   Example:
   (= (lens/update [1 2 3] mapped inc)
      [2 3 4])"
  (lens/traversal
   (fn [root]
     [root (fn [f] (map f root))])
   (fn [] "mapped")))

(defn- pred-map
  [pred? f collection]
  (for [item collection]
    (if (pred? item) (f item) item)))

(defn filtered
  "given a predicate gives you a traversal over
   the items of a seq that matches the predicate

   Examples:
   (= (lens/update [1 2 3] (filtered odd?) inc)
      [2 2 4])"
  [pred?]
  (lens/traversal
   (fn [root]
     [(filter pred? root)
      (fn [f] (pred-map pred? f root))])
   (fn [] "filtered")))

(def mapped-vals
  "a traversal over values in a map

   Example:
   (= (lens/update {:a 1, :b 2} mapped-vals inc)
      {:a 2, :b 3})"
  (lens/find-lens
   (fn [hash-map]
     (lens/comp-lenses [(lens/flatten-lenses (map lens/assoc-lens (keys hash-map)))
                        mapped]))
   (fn [] "mapped-vals")))

(defn- concat* [lens-a lens-b]
  (lens/traversal
   (fn [root]
     [(core/concat
       (lens/view root (->traversal lens-a))
       (lens/view root (->traversal lens-b)))
      (fn [f]
        (-> root
            (lens/update lens-a f)
            (lens/update lens-b f)))])
   (fn [] (str "concat[" (str lens-a) " " (str lens-b) "]"))))

(defn concat
  "takes a list of lenses/traversals and returns a
   traversal that is the `concated` traversal of all of them.

   view gives you all traversed items in the order of the lenses
   update writes to all travesed items in the order of the lenses"
  [lenses]
  (reduce concat* lens/ignore lenses))

(defn shrink
  "given a predicate shrink returns a traversal of 1 item
   that either includes the item or ignores the item.

   Examples:
   (= (lens/update 1 (shrink odd?) inc) 2)
   (= (lens/update 1 (shrink even?) inc) 1)

   (= (lens/update [1 2 3] [mapped (shrink even?)] inc)
      [1 3 3])"
  [pred?]
  (lens/traversal
   (fn [item]
     (let [match? (pred? item)]
       [(if match? [item] [])
        (fn [f]
          (if match? (f item) item))]))
   (fn [] "shrink")))
