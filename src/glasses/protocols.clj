(ns glasses.protocols)

(defprotocol Lensable
  (->lens [lens]
    "returns something that implements the Lens protocol"))

(defprotocol Lens
  (invoke-lens* [lens root]
    "invoke the lens on some root
     and returns a pair where the first item
     is the field that the lens points to.
     And the second item is a function that given a function
     of the field returns a new root with the updated field included in it")
  (comp-lens* [lens-top lens-bottom]
    "compose 2 lenses `lens-to` and `lens-bottom` and returns a new lens.
     That takes the root of the first lens and operates on the field of the second
     lens

     Lens[a,b] -> Lens[b, c] -> Lens[a, c]")
  (traversal? [lens]
    "returns true if the lens is a traversal"))

(defn invoke-lens
  "takes a Lensable and calls invoke-lens* on it after
   coercing it to a lens"
  [lens root]
  (invoke-lens* (->lens lens) root))

(defn comp-lens
  "takes 2 Lensables and cals comp-lens* on them
   after coercing them to lenses"
  [lens-top lens-bottom]
  (comp-lens* (->lens lens-top)
              (->lens lens-bottom)))
