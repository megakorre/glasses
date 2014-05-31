(ns glasses.protocols)

(defprotocol Lensable
  (->lens [lens]
    "returns something that implements
     the Lens protocol"))

(defprotocol Lens
  (invoke-lens* [lens root]
    "invoke the lens on some root")
  (comp-lens* [lens-top lens-bottom]
    "compose 2 lenses")
  (traversal? [lens]
    "returns wheter or not the lens is a traversal"))

(defn invoke-lens
  "lens[a,b] -> a -> [b, ((b -> c) -> a)]"
  [lens root]
  (invoke-lens* (->lens lens) root))

(defn comp-lens
  "lens[a,b] -> lens[b,c] -> lens[a,c]"
  [lens-top lens-bottom]
  (comp-lens* (->lens lens-top)
              (->lens lens-bottom)))
