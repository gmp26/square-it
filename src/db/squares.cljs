(ns ^:figwheel-always db.squares
    (:require [datascript :as d]))

(enable-console-print!)

(defn square [x y dx dy]
  "describes a square at bottom-left [x y] offset [dx dy] to bottom-right"
  (let [x2 (+ x dx) 
        y2 (+ y dy)] 
    [[x y]
     [(+ x dx) (+ y dy)]
     [(+ x dx (- dy)) (+ y dx dy)]
     [(- x dy) (+ x dx)]]))

(defn sq [x y dx dy]
  [x y dx dy])

(defn onboard? [n s]
  "true iff square s is inside square board of size n"
  (let [inside? (fn [[x y]] (and (< x n) (< y n) (>= x 0) (>= y 0)))]
    (every? inside? s)))

(defn all-squares [n]
  "generates all possible squares for a square board of size n"
  (let [n1 (- n 1)
        n2 (inc n)]
    (filter #(onboard? n %)
            (apply concat
                   (for [x (range n1)]
                     (apply concat
                            (for [y (range n1)]
                              (apply concat 
                                     (for [dx (range 1 (- n2 x))]
                                       (for [dy (range 0 (- n2 y dx))]
                                         (square x y dx dy)))))))))))

;;
;; square db
;;
;; Implicit join, multi-valued attribute

(def conn 
  (d/create-conn 
   {:p {:db/cardinality :db.cardinality/many}}
))

(let [squares (all-squares 3)]
  (d/transact! conn 
               (map-indexed (fn [ix sq]
                              {
                               :color :red
                               :name ix
                               :p sq})
                            squares))
  (prn (d/q '[:find (aggregate ?p) ?p ?color
              :where
              [?e :color ?color]
              [?e :p ?p]
              ]
            @conn))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sample db stuff ;;;;;;;;;;;


;; reference


(let [schema {:aka {:db/cardinality :db.cardinality/many}}
      conn   (d/create-conn schema)]
  (d/transact! conn [ { :db/id -1
                       :name  "Maksim"
                       :age   45
                       :aka   ["Maks Otto von Stirlitz", "Jack Ryan"] } ])
  (d/q '[ :find  ?n ?a
         :where [?e :aka "Maks Otto von Stirlitz"]
         [?e :name ?n]
         [?e :age  ?a] ]
       @conn))

;; => #{ ["Maksim" 45] }


;; Destructuring, function call, predicate call, query over collection

(defn testdb [] (d/q '[ :find  ?k ?x
                       :in    [[?k [?min ?max]] ...] ?range
                       :where [(?range ?min ?max) [?x ...]]
                       [(even? ?x)] ]
                     { :a [1 7], :b [2 4] }
                     range))

;; => #{ [:a 2] [:a 4] [:a 6] [:b 2] }


;; Recursive rule

(d/q '[ :find  ?u1 ?u2
       :in    $ %
       :where (follows ?u1 ?u2) ]
     [ [1 :follows 2]
       [2 :follows 3]
       [3 :follows 4] ]
     '[ [(follows ?e1 ?e2)
         [?e1 :follows ?e2]]
        [(follows ?e1 ?e2)
         [?e1 :follows ?t]
         (follows ?t ?e2)] ])

;; => #{ [1 2] [1 3] [1 4]
;;       [2 3] [2 4]
;;       [3 4] }


;; Aggregates

(prn (d/q '[ :find ?color (max ?amount ?x) (min ?amount ?x)
            :in   [[?color ?x]] ?amount ]
          [[:red 10]  [:red 20] [:red 30] [:red 40] [:red 50]
           [:blue 7] [:blue 8]]
          1))

;; => [[:red  [30 40 50] [10 20 30]]
;;     [:blue [7 8] [7 8]]]


;; end commented region


