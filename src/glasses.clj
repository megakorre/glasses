(ns glasses
  (:require [glasses.protocols :refer [comp-lens invoke-lens traversal? Lensable Lens]]))

(declare lens traversal view update)

(defn- comp-lens-impl
  [lens-top lens-bottom]
  ((if (traversal? lens-bottom) traversal lens)
   (fn [root]
     (let [[field-top    top-f]    (invoke-lens lens-top    root)
           [field-bottom bottom-f] (invoke-lens lens-bottom field-top)]
       [field-bottom
        (fn [f] (top-f (fn [_] (bottom-f f))))]))))

(defn- comp-traversal-impl
  [lens-top lens-bottom]
  (traversal
   (fn [root]
     (let [[collection top-f] (invoke-lens lens-top root)]
       [((if (traversal? lens-bottom) mapcat map)
         (fn [field] (view field lens-bottom))
         collection)
        (fn [f]
          (top-f (fn [field] (update field lens-bottom f))))]))))

(defn lens [f]
  (reify
    Lensable
    (->lens [lens] lens)
    Lens
    (traversal? [_] false)
    (invoke-lens* [_ root] (f root))
    (comp-lens* [lens-top lens-bottom]
      (comp-lens-impl lens-top lens-bottom))))

(defn traversal [f]
  (reify
    Lensable
    (->lens [traversal] traversal)
    Lens
    (traversal? [_] true)
    (invoke-lens* [_ root] (f root))
    (comp-lens* [lens-top lens-bottom]
      (comp-traversal-impl lens-top lens-bottom))))

(defn view
  "a -> lens[a,b] -> b"
  [root lens]
  (let [[value root-f] (invoke-lens lens root)]
    value))

(defn update
  "a -> lens[a,b] -> (b -> c) -> lens[a,c]"
  [root lens f & args]
  (let [[value root-f] (invoke-lens lens root)]
    (root-f (fn [v] (apply f v args)))))

(defn write
  "a -> lens[a,b] -> b -> a"
  [root lens value]
  (update root lens (constantly value)))

(def id
  "lens[a, a]"
  (lens
   (fn [root]
     [root (fn [f] (f root))])))

(defn- mod-val
  [map key f]
  (let [old-field (get map key)
        new-field (f old-field)]
    (if (nil? new-field)
      (dissoc map key)
      (assoc  map key new-field))))

(defn assoc-lens
  "k -> lens[map, val]"
  [key]
  (lens
   (fn [root]
     [(get root key) (fn [f] (mod-val root key f))])))

(defn comp-lenses
  "[lens[a,b?]...] -> lens[a,b]"
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
  "{as: lens[a,?x]...} -> lens[a, {as: ?x...}]"
  [lens-map]
  (lens
   (fn [root]
     (let [field (view-keys root lens-map)]
       [field (partial extract-root-f lens-map root field)]))))

(defn flatten-lenses
  "[lens[a,b]..] -> lens[a, [b]]"
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
             (map vector new-fields fields lenses))))]))))

(defn find-lens
  "(a -> lens[a,b]) -> lens[a,b]"
  [f]
  (lens
   (fn [root]
     (invoke-lens (f root) root))))

(defn getter-setter-lens
  "(a -> b) -> (a -> b -> a) -> lens[a,b]"
  [getter setter]
  (lens
   (fn [root]
     (let [field (getter root)]
       [field (fn [f] (setter root (f field)))]))))

(defn iso-lens
  "(a -> b) -> (b -> a) -> lens[a,b]"
  [a->b b->a]
  (getter-setter-lens a->b (fn [_ b] (b->a b))))

(extend-protocol Lensable
  clojure.lang.Keyword
  (->lens [keyword] (assoc-lens keyword))
  clojure.lang.PersistentVector
  (->lens [vector]  (comp-lenses vector))
  clojure.lang.PersistentArrayMap
  (->lens [hash]    (extract hash))
  clojure.lang.PersistentHashMap
  (->lens [hash]    (extract hash)))
