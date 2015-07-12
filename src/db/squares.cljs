(ns ^:figwheel-always db.squares
    (:require [datascript :as d]
              [cljs-uuid.core :as uuid]))

;;
;; See http://docs.datomic.com/clojure for api superset (datomic java verion)
;;
;; Transactions in http://docs.datomic.com/transactions.html
;;
;; http://www.slideshare.net/fractallambda/datascript-and-reagent for slides
;;
;; https://github.com/tonsky/datascript/issues/46 for explanation of :db/id
;;

(enable-console-print!)

;;
;; bind a database change to a rum/react atom
;;
(defn bind
  ([conn q]
    (bind conn q (atom nil)))
  ([conn q state]
    (let [k (uuid/make-random)]
      (reset! state (d/q q @conn))
      (d/listen! conn k (fn [tx-report]
                          (let [novelty (d/q q (:tx-data tx-report))]
                            (when (not-empty novelty)
                              (reset! state (d/q q (:db-after tx-report)))))))
      (set! (.-__key state) k)
      state)))

(defn unbind
  [conn state]
  (d/unlisten! conn (.-__key state)))


;;
;; square generation
;;
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

(defn all-points [n]
  "generates all points on the square-it board"
  (for [i (range 0 n)
        j (range 0 n)]
       [i j]) )

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

;; Create database connection. it's an in-memory atom.
(def conn 
  (d/create-conn 
   {:square/point {:db/cardinality :db.cardinality/many
                   :db/valueType :db.type/ref
                   }}
))

;; create points (x,y) with colour c :none
(defn db-add-points [n]
  (d/transact! conn (map (fn [p] {:point/p p
                                  :point/c :none}) (all-points n)))
  )


(defn point-at [p]
  (first (first (d/q '[:find ?pe
                       :in $ ?p
                       :where
                       [?pe :point/p ?p]
                       ] @conn p))))

(defn find-point [p]
  (let [pe (d/q '[:find ?pe 
                  :in $ ?p
                  :where 
                  [?pe :point/p ?p]
                  ] @conn p)]
    pe))

(defn points [[p1 p2 p3 p4]]
  (d/q '[:find ?pe1 ?pe2 ?pe3 ?pe4
         :in $ ?p1 ?p2 ?p3 ?p4
         :where 
         [?pe1 :point/p ?p1]
         [?pe2 :point/p ?p2]
         [?pe3 :point/p ?p3]
         [?pe4 :point/p ?p4]
         ] @conn p1 p2 p3 p4))

(defn squares-by-pe [n]
  (map first (map points (all-squares n))))

(defn db-add-squares [n]
  (db-add-points n)
  (d/transact! conn 
               (map (fn [sbpe] {:square/point sbpe}) (squares-by-pe n)))
  nil)


(defn count-frees []
  (d/q '[:find (count ?s)
         :where
         [?s :square/point _]] @conn ))

(defn frees []
  (d/q '[:find (count ?pe)
         :where
         [?s :square/point ?pe]
         [?pe :point/c :none]
         ] @conn ))

(defn get-point-fill [p]
  (d/q '[:find ?fill
         :in $ ?pe
         :where
         [?pe :point/c ?fill]] @conn (point-at p))) 

(defn occupy-point [p c]
  (let [pe (point-at p)]
    (if pe 
      (do
        (d/transact! conn [{:db/retract (point-at p)
                            :point/c c}])
        (d/transact! conn [{:db/id (point-at p)
                            :point/c c}]))
      (prn (str p " not found")))
    )
  nil
  )


;; This works
(defn test-ref1 []
  (db-add-points 2)
  (d/transact! conn [{:db/id -1 :square/point 1}])
  (d/transact! conn [{:db/id -1 :square/point 2}])
  (d/transact! conn [{:db/id -1 :square/point 3}])
  (d/transact! conn [{:db/id -1 :square/point 4}])
  (d/q '[:find ?p :where [?s :square/point ?pe] [?pe :point/p ?p]] @conn))
;; => #{[[0 0]] [[0 1]]}

(defn test-ref2 []
  (db-add-points 2)
  (d/transact! conn [{:square/point [1 2 3 4]}])
  (d/q '[:find ?p :where [?s :square/point ?pe] [?pe :point/p ?p]] @conn))


#_(defn  one-colour-q [c]
  (d/q find '[?s
              :in $ ?c
              :where
              [?s :square/id ?id]
              [?s :square/point ?p]
              [?s :square/colour ?c]] squares-db c) 
)

#_(defn  two-colour-q [c1 c2]
  (d/q find '[?s
              :in $ ?color
              :where
              [?s :square/id ?id]
              [?s :square/point ?p]
              [?s :square/colour ?c1]
              [?s :square/colour ?c2]] squares-db c1 c2) 
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reference example commented out
;;
(comment
  (prn (str (let [schema2 {:aka {:db/cardinality :db.cardinality/many}}
                  conn2   (d/create-conn schema2)]
              (d/transact! conn [ {:db/id -1
                                   :name  "Maksim"
                                   :age   45
                                   :aka   ["Maks Otto von Stirlitz", "Jack Ryan"] } ])
              (d/q '[ :find  ?n ?a
                     :where [?e :aka "Maks Otto von Stirlitz"]
                     [?e :name ?n]
                     [?e :age  ?a] ]
                   @conn2)

              )))

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
            4))

  ;; => [[:red  [30 40 50] [10 20 30]]
  ;;     [:blue [7 8] [7 8]]]


  ;; end commented region


  )
