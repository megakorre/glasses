(ns glasses
  (:require [glasses.protocols :refer [comp-lens invoke-lens traversal?] :as proto]))

(declare lens traversal view update)

(defn- comp-lens-impl
  [lens-top lens-bottom]
  ((if (traversal? lens-bottom) traversal lens)
   (fn [root]
     (let [[field-top    top-f]    (invoke-lens lens-top    root)
           [field-bottom bottom-f] (invoke-lens lens-bottom field-top)]
       [field-bottom
        (fn [f] (top-f (fn [_] (bottom-f f))))]))
   (fn [] (str lens-top " " lens-bottom))))

(defn- comp-traversal-impl
  [lens-top lens-bottom]
  (traversal
   (fn [root]
     (let [[collection top-f] (invoke-lens lens-top root)]
       [((if (traversal? lens-bottom) mapcat map)
         (fn [field] (view field lens-bottom))
         collection)
        (fn [f]
          (top-f (fn [field] (update field lens-bottom f))))]))
   (fn [] (str lens-top " " lens-bottom))))

(deftype Lens [f lens-name-f]
  Object
  (toString [this] (if lens-name-f (lens-name-f) ""))
  proto/Lensable
  (->lens [lens] lens)
  proto/Lens
  (traversal? [_] false)
  (invoke-lens* [_ root] (f root))
  (comp-lens* [lens-top lens-bottom]
    (comp-lens-impl lens-top lens-bottom)))

(defn lens
  "takes a \"lens function\" and returns a lens
   a lens function is a function that given a root
   returns a pair where the first item is the field the lens points to
   and the second item is a function that given a function returns a new
   root with the field updated

   also takes a optional function `lens-name-f` that is called when
   the lens is printed"
  [f & [lens-name-f]]
  (Lens. f lens-name-f))

(deftype Traversals [f lens-name-f]
  Object
  (toString [this] (if lens-name-f (lens-name-f) ""))
  proto/Lensable
  (->lens [traversal] traversal)
  proto/Lens
  (traversal? [_] true)
  (invoke-lens* [_ root] (f root))
  (comp-lens* [lens-top lens-bottom]
    (comp-traversal-impl lens-top lens-bottom)))

(defn traversal
  "takes a \"traversal function\" and returns a traversal

   a traversal function is a function that takes a root
   and returns a pair.
   The first item in that pair is a seq of all items that the traversal
   points to.
   And the second item in that pair is a function, that given a function of the items
   traversed returns a new root with the items updated by that function"
  [f & [lens-name-f]]
  (Traversals. f lens-name-f))

(defn view
  "takes a root and a lens and returns the field
   that the lens points to"
  [root lens]
  (let [[value root-f] (invoke-lens lens root)]
    value))

(defn update
  "takes a root a lens and a function (and optional extra arguments to that function)
   and returns a new root with the field that the lens points to updated with the given function."
  [root lens f & args]
  (let [[value root-f] (invoke-lens lens root)]
    (root-f (fn [v] (apply f v args)))))

(defn write
  "takes a root, lens and a value and replaces the field the lens points
   to with that value"
  [root lens value]
  (update root lens (constantly value)))

(def id
  "the id lens is a lens that is a lens of the root itself.
   if viewed it returns the root. And if uppdated it updates the root."
  (lens
   (fn [root]
     [root (fn [f] (f root))])
   (fn [] "id")))

(def ignore
  "ignore is a traversal that returns no items if viewed
   and ignores the function given if updated"
  (traversal
   (fn [root]
     [[] (fn [_] root)])
   (fn [] "ignore")))

(defn- mod-val
  [map key f]
  (let [old-field (get map key)
        new-field (f old-field)]
    (if (nil? new-field)
      (dissoc map key)
      (assoc  map key new-field))))

(defn assoc-lens
  "assoc lens takes a key and returns a lens
   that points to the value of any associtive structure
   that is associated with that key"
  [key]
  (lens
   (fn [root]
     [(get root key) (fn [f] (mod-val root key f))])
   (fn [] (str "assoc[" key "]"))))

(defn comp-lenses
  "takes a list of lenses and composes them from first to last"
  [lenses]
  (reduce comp-lens id lenses))

(defn- extract-map-merger
  [new-map old-map acu [key lens]]
  (let [new-val (get new-map key)]
    (if (identical? new-val (get old-map key))
      acu
      (write acu lens new-val))))

(defn- extract-root-f [lens-map root old-map f]
  (let [new-map (f old-map)
        merger  (partial extract-map-merger new-map old-map)]
    (reduce merger root lens-map)))

(defn- view-keys
  "takes a map where the values are lenses
   and a root and returns a map with the keys of the
   input map mapped to the values of viewing the root with the lenses"
  [root lens-map]
  (into {} (for [[k lens] lens-map]
             [k (view root lens)])))

(defn extract
  "extract takes a map where the values are lenses
   from the same root to different or the same fields.
   And returns a lens
   The lens is a lens from the shared root of the input lenses
   to a map where the values are the fields the lenses pointed to.
   changes to these values are updated in the original root when updated
   in the map"
  [lens-map]
  (lens
   (fn [root]
     (let [field (view-keys root lens-map)]
       [field (partial extract-root-f lens-map root field)]))
   (fn [] (str lens-map))))

(defn flatten-lenses
  "takes a list of lenses from the same root and returns a lens.
   where the field is a list of all fields in the given list.
   updates to these fields are reflected in the given root"
  [lenses]
  (lens
   (fn [root]
     (let [fields (for [lens lenses] (view root lens))]
       [fields
        (fn [f]
          (let [new-fields (f fields)]
            (assert (= (count new-fields) (count lenses)))
            (reduce
             (fn [acu [new-value old-value lens]]
               (if (identical? new-value old-value)
                 acu
                 (write acu lens new-value)))
             root
             (map vector new-fields fields lenses))))]))
   (fn [] (str lenses))))

(defn find-lens
  "takes a function from a root to a lens for that root.
   And returns a lens where the field is the field of the lens
   returned by the function"
  [f & [lens-name]]
  (lens
   (fn [root]
     (invoke-lens (f root) root))
   (fn [] (or lens-name "find-lens"))))

(defn getter-setter-lens
  "takes a getter and a setter function and returns a lens"
  [getter setter & [lens-name]]
  (lens
   (fn [root]
     (let [field (getter root)]
       [field (fn [f] (setter root (f field)))]))
   (fn [] (or lens-name "getter-setter"))))

(defn iso-lens
  "takes 2 functions that can convert a value back and forth.
   And returns a lens that where the field is the converted value"
  [a->b b->a]
  (getter-setter-lens a->b (fn [_ b] (b->a b)) "iso-lens"))

(extend-protocol proto/Lensable
  clojure.lang.Keyword
  (->lens [keyword] (assoc-lens keyword))
  clojure.lang.PersistentVector
  (->lens [vector]  (comp-lenses vector))
  clojure.lang.PersistentArrayMap
  (->lens [hash]    (extract hash))
  clojure.lang.PersistentHashMap
  (->lens [hash]    (extract hash)))
