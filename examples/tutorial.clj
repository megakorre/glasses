(require '[glasses :as lens])
(require '[glasses.traversals :as travers])
(require '[glasses.tree :as tree])

;; #######################################
;; basic lens

(def user
  {:first-name "tobias"
   :last-name "funke"
   :email "tobias.funke@gmail.com"})

;; this is the raw way to define a lens
;; you call lens/lens and give it a function
;; that takes a root value in this case a user
;; and returns a pair where the first value
;; is the field. And the second value is a function
;; that takes a function of the name and returns a new user

(def first-name
  (lens/lens
   (fn [user]
     (let [name (:first-name user)]
       [name
        (fn [f] (assoc user :first-name (f name)))]))))

(require '[clojure.string :as string])
(use 'clojure.pprint)

;; you can get the field from the root value
;; by calling view on it with the lens

(pprint (lens/view user first-name))
;; => "tobias"

;; you can also update the field with the update function

(pprint (lens/update user first-name string/capitalize))
;; => {:last-name "funke",
;;     :first-name "Tobias", <-- capitalized
;;     :email "tobias.funke@gmail.com"}

;; you can also write to the field with lens/write
;; this is the same as doing (update root lens (fn [_] new-value))

(pprint (lens/write user first-name "Newname"))
;; => {:last-name "funke",
;;     :first-name "Newname",
;;     :email "tobias.funke@gmail.com"}

;; you can also define lenses using lens/getter-setter-lens
;; which makes a lens from a getter and a setter function

(def first-name2
  (lens/getter-setter-lens
   (fn [user] (:first-name user))
   (fn [user new-first-name] (assoc user :first-name new-first-name))))

;; this will make you the same lens
(= (lens/write user first-name  "Newname")
   (lens/write user first-name2 "Newname"))

;; since using keywords to reference fields in maps is so common
;; you can use them directly as lenses

(= (lens/write user first-name  "Newname")
   (lens/write user first-name2 "Newname")
   (lens/write user :first-name "Newname"))

(= (lens/view user first-name)
   (lens/view user first-name2)
   (lens/view user :first-name))

;; ========================================================
;; composition

;; a critical future of lenses are that they compose
;; you can do this with the lens/comp-lenses
;; this function takes a seq of lenses and combines them

(def user2
  {:first-name "tobias"
   :last-name "funke"
   :address {:city "bluethwill"
             :address_line "sudden vale street 23"
             :postcode "56321"}})

(def user-city (lens/comp-lenses [:address :city]))

(= (lens/view user2 user-city) "bluethwill")

;; if you use a vector of lenses as a lens
;; you will get the same result as calling lens/comp-lenses on the lenses

(= (lens/view user2 [:address :postcode])
   "56321")

;; so basically you can use lenses just like you use
;; the clojure *-in functions

(= (get-in user2 [:address :city])
   (lens/view user2 [:address :city]))

(= (update-in user2 [:address :city] string/capitalize)
   (lens/update user2 [:address :city] string/capitalize))

;; =====================================================
;; iso-lens

;; the postcode in the nested structure is a string atm
;; but we can write a lens that lets us treat it as a number

(def str->num (lens/iso-lens #(Integer/parseInt %) str))

;; iso-lens takes 2 functions one to convert it to a integer in this case
;; and another one to convert it back to a string

(lens/update user2 [:address :postcode str->num] inc)
;; {:last-name "funke",
;;  :first-name "tobias",
;;  :address {:address_line "sudden vale street 23",
;;            :postcode "56322",
;;            :city "bluethwill"}}

;; the result after incrementing the postcode is still a string

;; =====================================================
;; extract

;; extract is a function that lets you transform a structure
;; to a map of a different shape and later write back changes
;; that are done to the new map

(def user-view
  {:first-name   :first-name
   :last-name    :last-name
   :address_line [:address :address_line]
   :address_city [:address :city]})

(= (lens/view user2 user-view)
   {:address_line "sudden vale street 23",
    :last-name "funke",
    :first-name "tobias",
    :address_city "bluethwill"})

(defn take-ower-town
  "claim the town as you own"
  [user] ;; user in the user-view shape
  (assoc user :address_city (:first-name user)))

(= (lens/update user2 user-view take-ower-town)
   {:last-name "funke", :first-name "tobias",
    :address {:address_line "sudden vale street 23",
              :postcode "56321",
              :city "tobias"}})

;; =================================================
;; traversals

;; traversals lets you dig into collections (or any thing that can be "traversed")

(def user3
  {:first-name "tobias"
   :last-name  "funke"
   :languages ["clojure" "ruby" "elixir"]})

(= (lens/update user3 [:languages travers/mapped] string/capitalize)
   {:last-name "funke",
    :first-name "tobias",
    :languages '("Clojure" "Ruby" "Elixir")})

;; travers/mapped treats the items as seqs. so all results will be
;; converted to lists
;; but a traversal can create whatever structure it wants

;; lets write a vector travers

(def mapped-vec
  (lens/traversal
   (fn [collection]
     [collection
      (fn [f] (mapv f collection))])))

;; traversals are built a lot like lenses
;; its a function that takes the structure
;; in this case a collection
;; it returns a pair that where the first item is
;; all the items it "traverses" as a seq.
;; and the second item is a function
;; that builds a new value after applying the function
;; to every item it traverses. IE a mapping

(= (lens/update user3 [:languages mapped-vec] string/capitalize)
   {:last-name "funke",
    :first-name "tobias",
    :languages ["Clojure" "Ruby" "Elixir"]})

;; this version keeps the languages in a vector

;; traversals can be composed with lenses in any way you want

(def deep-data
  {:a [{:b [{:c [1 2 3]}
            {:c [3 4 5]}]}
       {:b [{:c [9 8 7]}
            {:c []}]}
       {:b [{:c [1 3]}
            {:c [3]}]}]})

(def cs-lens [:a mapped-vec :b mapped-vec :c mapped-vec])

(= (lens/update deep-data cs-lens * 2)
   {:a [{:b [{:c [2 4 6]}
             {:c [6 8 10]}]}
        {:b [{:c [18 16 14]}
             {:c []}]}
        {:b [{:c [2 6]}
             {:c [6]}]}]})

;; you can take a traversal and shrink the result your working with

(def even-cs-lens [cs-lens (travers/shrink even?)])

(= (lens/write deep-data even-cs-lens 0)
   {:a [{:b [{:c [1 0 3]}
             {:c [3 0 5]}]}
        {:b [{:c [9 0 7]}
             {:c []}]}
        {:b [{:c [1 3]}
             {:c [3]}]}]})

;; this will only write to even values

;; if you view a traversed lens you get all
;; values it points to as a flattened seq

(= (lens/view deep-data even-cs-lens)
   [2 4 8])

;; this returns all even values in the :c fields of the deep-data

;; ===========================================

;; tree
;; ok! lets work with some tree data
;; courtesy of http://msdn.microsoft.com/en-us/library/ms762271(v=vs.85).aspx

(def xml-string
  "<?xml version=\"1.0\"?>
   <catalog>
   <book id=\"bk101\">
      <author>Gambardella, Matthew</author>
      <title>XML Developer's Guide</title>
      <genre>Computer</genre>
      <price>44.95</price>
      <publish_date>2000-10-01</publish_date>
      <description>An in-depth look at creating applications
      with XML.</description>
   </book>
   <book id=\"bk102\">
      <author>Ralls, Kim</author>
      <title>Midnight Rain</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-12-16</publish_date>
      <description>A former architect battles corporate zombies,
      an evil sorceress, and her own childhood to become queen
      of the world.</description>
   </book>
   <book id=\"bk103\">
      <author>Corets, Eva</author>
      <title>Maeve Ascendant</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2000-11-17</publish_date>
      <description>After the collapse of a nanotechnology
      society in England, the young survivors lay the
      foundation for a new society.</description>
   </book>
   <book id=\"bk104\">
      <author>Corets, Eva</author>
      <title>Oberon's Legacy</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-03-10</publish_date>
      <description>In post-apocalypse England, the mysterious
      agent known only as Oberon helps to create a new life
      for the inhabitants of London. Sequel to Maeve
      Ascendant.</description>
   </book>
   <book id=\"bk105\">
      <author>Corets, Eva</author>
      <title>The Sundered Grail</title>
      <genre>Fantasy</genre>
      <price>5.95</price>
      <publish_date>2001-09-10</publish_date>
      <description>The two daughters of Maeve, half-sisters,
      battle one another for control of England. Sequel to
      Oberon's Legacy.</description>
   </book>
   <book id=\"bk106\">
      <author>Randall, Cynthia</author>
      <title>Lover Birds</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-09-02</publish_date>
      <description>When Carla meets Paul at an ornithology
      conference, tempers fly as feathers get ruffled.</description>
   </book>
   <book id=\"bk107\">
      <author>Thurman, Paula</author>
      <title>Splish Splash</title>
      <genre>Romance</genre>
      <price>4.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>A deep sea diver finds true love twenty
      thousand leagues beneath the sea.</description>
   </book>
   <book id=\"bk108\">
      <author>Knorr, Stefan</author>
      <title>Creepy Crawlies</title>
      <genre>Horror</genre>
      <price>4.95</price>
      <publish_date>2000-12-06</publish_date>
      <description>An anthology of horror stories about roaches,
      centipedes, scorpions  and other insects.</description>
   </book>
   <book id=\"bk109\">
      <author>Kress, Peter</author>
      <title>Paradox Lost</title>
      <genre>Science Fiction</genre>
      <price>6.95</price>
      <publish_date>2000-11-02</publish_date>
      <description>After an inadvertant trip through a Heisenberg
      Uncertainty Device, James Salway discovers the problems
      of being quantum.</description>
   </book>
   <book id=\"bk110\">
      <author>O'Brien, Tim</author>
      <title>Microsoft .NET: The Programming Bible</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-09</publish_date>
      <description>Microsoft's .NET initiative is explored in
      detail in this deep programmer's reference.</description>
   </book>
   <book id=\"bk111\">
      <author>O'Brien, Tim</author>
      <title>MSXML3: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>36.95</price>
      <publish_date>2000-12-01</publish_date>
      <description>The Microsoft MSXML3 parser is covered in
      detail, with attention to XML DOM interfaces, XSLT processing,
      SAX and more.</description>
   </book>
   <book id=\"bk112\">
      <author>Galos, Mike</author>
      <title>Visual Studio 7: A Comprehensive Guide</title>
      <genre>Computer</genre>
      <price>49.95</price>
      <publish_date>2001-04-16</publish_date>
      <description>Microsoft Visual Studio 7 is explored in depth,
      looking at how Visual Basic, Visual C++, C#, and ASP+ are
      integrated into a comprehensive development
      environment.</description>
   </book>
</catalog>")

(require '[clojure.xml :as xml])

(defn parse-xml-str [xml-text]
  (xml/parse
   (java.io.ByteArrayInputStream.
    (.getBytes xml-text))) )

(def xml-catalog (parse-xml-str xml-string))

;; first task! something simple
;; Lets change the name of all the authors to tobias funke in this xml document.
;; This could be accomplished by digging down to the right elements and updating them
;; appropriately... but a lazier way would be to just walk all of the tree
;; and replace the contents of the elements with the author tag

;; we can use tree/pre-tree to accomplish this
;; it is a function that takes a traversal of all sub nodes of a node
;; and gives you a traversal of all nodes in the tree
;; EXAMPLE:

;; since nodes in the

(def xml-nodes (tree/pre-tree
                [:content
                 (travers/filtered map?) ; we don't want to list the strings
                                         ; in the tree as nodes
                                         ; so we filter them here
                 ]))

;; xml nodes here is a lens/traversal that lets you view and modify
;; nodes of the tree in a pre-walk fashion.
;; (there is also a post-tree that lets you do it in a post-walk fashion)

;; ok so lets shrink the nodes we are working with to only the nodes
;; with the tag author

(def authors [xml-nodes (travers/shrink #(= (:tag %) :author))])

;; and change the contents of these nodes to be "tobias funke"

(pprint
 (-> xml-catalog
     (lens/write [authors :content] ["tobias funke"])))

;; perfect all the authors are now tobias funke
