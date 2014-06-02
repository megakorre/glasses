(ns glasses.tree
  (:require [glasses            :as lens]
            [glasses.traversals :as travers]
            [clojure.walk       :as walk]))

(defn- view-tree [traversal node]
  (cons node (mapcat (partial view-tree traversal)
                     (lens/view node traversal))))

(defn- pre-replace-tree
  [traversal f node]
  (lens/update
   (f node)
   traversal
   (partial pre-replace-tree traversal f)))

(defn pre-tree
  "given a traversal from a node to its items of nodes
   pre-tree returns a traversal of all nodes in the tree
   where updates to the tree are done before diggin down the tree

   Examples:

   (= (lens/update {:v 1, :n [{:v 2}, {:v 3}]}
       [(pre-tree [:n travers/mapped]) :v]
       inc)
      {:v 2, :n [{:v 3, :n []} {:v 4, :n []}]})"
  [traversal]
  (lens/traversal
   (fn [node]
     [(view-tree traversal node)
      (fn [f] (pre-replace-tree traversal f node))])))

(defn- post-replace-tree
  [traversal f node]
  (f (lens/update
      node
      traversal
      (partial post-replace-tree traversal f))))

(defn post-tree
  "given a traversal from a node to its items of nodes
   pre-tree returns a traversal of all nodes in the tree
   where updates to the tree are done after diggin down the tree

   Examples:

   (= (lens/update {:v 1, :n [{:v 2}, {:v 3}]}
       [(post-tree [:n travers/mapped]) :v]
       inc)
      {:v 2, :n [{:v 3, :n []} {:v 4, :n []}]})"
  [traversal]
  (lens/traversal
   (fn [node]
     [(view-tree traversal node)
      (fn [f] (post-replace-tree traversal f node))])))
