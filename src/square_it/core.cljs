(ns ^:figwheel-always square-it.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (union)]
              [cljs.pprint :refer (pprint)]
              [cljsjs.react])
    )

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; square and point generation
;;
(defn square [n x y dx dy]
  "describes a square at bottom-left [x y] offset [dx dy] to bottom-right. False if outside board bounds."
  (let [x2 (+ x dx)]
    (if (< x2 n)
      (let  [y2 (+ y dy)]
        (if (< y2 n)
          (let [x3 (- x2 dy)]
            (if (>= x3 0)
              (let [y3 (+ y2 dx)]
                (if (< y3 n)
                  (let [x4 (- x dy)]
                    (if (>= x4 0)
                      (let [y4 (+ y dx)]
                        (if (< y4 n)
                          #{[x y] [x2 y2] [x3 y3] [x4 y4]})))))))))))))

;;; "100 Elapsed time: 1187 msecs" (28 -> 1101)
(defn all-squares [n]
  "generates all possible squares for a square board of size n"
  (let [n1 (- n 1)
        n2 (inc n)]
    (mapcat
     (fn [x]
       (mapcat 
        (fn [y]
          (mapcat 
           (fn [dx] 
             (remove nil?
                     (map 
                      (fn [dy] (square n x y dx dy))        
                      (range 0 (- n2 y dx))))) 
           (range 1 (- n2 x))))
        (range n1)))
     (range n1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; state constants
;;
(prn "-- Restart --")

(def initial-state {:n 5
                    :player :a
                    :players 1
                    :a-first true
                    :as #{}
                    :bs #{}
                    :squares (all-squares (:n initial-state))
                    })

(defonce game (atom initial-state))

(def messages {:yours "Your turn"
               :als   "Al's turn"
               :as-turn "Player A's turn"
               :bs-turn "Player B's turn"
               :you-win "Well done! You won"
               :al-win "Oops! You lost"
               :a-win "Player A won"
               :b-win "Player B won"
               :draw  "It's a draw!"
               })

(def message-colours {:yours :a
                      :als   :b
                      :as-turn :a
                      :bs-turn :b
                      :you-win :a
                      :al-win :b
                      :a-win :a
                      :b-win :b
                      :draw :draw
                      })

(def bound-width 320)
(def bound-height 320)
(def max-n 9)
(def min-n 3)
(def unit 1)
(def gap 36)
(defn units [x] (* x unit))
(defn gaps [x] (* (units (+ x 0.5)) gap))
(def tick-interval 1000)
(def al-think-time 2000)


(def player-colours {:a "rgb(0, 153, 255)"
                     :b "rgb(238, 68, 102)"
                     :none "rgb(220,255,220)"
                     :draw "rgb(74, 157, 97)"
                     })

(def player-class {:a "blue"
                   :b "red"
                   :none "grey"
                   :draw "draw"
                   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; strategy utils
;;

(defn unpainted? [g point]
  (let [as (:as g)
        bs (:bs g)]
    (if (not (or (as point) (bs point))) true nil)))

(defn unclaimed? [g points-in-square]
  (every? #(unpainted? g %) points-in-square))

(defn empty-squares [g]
  (filter #(unclaimed? g %) (:squares g)))

;
; ****
;
(defn raw-corner-count [square player-points]
  "count corners of a square belonging to one player"
  (->> square
       (map #(if (player-points %) 1 0))
       (reduce +) ))

(defn good-square-corners [as bs square]
  "find and score free corners in singly-owned squares"
  (let  [as-count (raw-corner-count square as)
         bs-count (raw-corner-count square bs)]
    (filter #(and (not (as %)) (not (bs %)) (= 0 (* as-count bs-count))) square)))

(defn good-corners [as bs squares]
  (map #(good-square-corners as bs %) squares)
)

(defn point-scores []
  "count and score the number of makeable squares that each point can influence"
  ())

(defn square-potential [as bs square]
  "return [a b] ownership counts of a square. A doubly owned square returns nil"
  (let [as-count (raw-corner-count square as)
        bs-count (raw-corner-count square bs)]
    (if (= 0 (* as-count bs-count))
      [as-count bs-count]
      nil
      )))

(defn squares-potential [as bs squares]
  "return [a b] ownership counts of squares. A doubly owned square returns nil"
  (map #(square-potential as bs %) squares)
)

(defn game-potential [g]
  "return [a b] ownership counts of squares in a game. A doubly owned square returns nil"
  (map #(square-potential (:as g) (:bs g) %) (:squares g))
)

(defn game-drawn? [g]
  (every? nil? (game-potential g)))

(defn game-over? [g]
  (let [pot (game-potential g)]
    (or 
     (some #(or (= 4 (first %)) (= 4 (second %))) pot)
     (game-drawn? g))))

(defn is4 [i n] (if (= 4 n) i nil))
(defn is3 [i n] (if (= 3 n) i nil))
(defn is2 [i n] (if (= 2 n) i nil))
(defn is1 [i n] (if (= 1 n) i nil))

;;
;; debugger
;;
(defn deb [x] 
  (prn x)
  x)

(defn get-tactic [potential player]
  (let [px (if (= player :a) 0 1)
        opx (- 1 px)
        p-counts (map #(nth % px) potential)
        op-counts (map #(nth % opx) potential)]

    (prn (str "potential " potential))
    (prn (str "player" player))
    (prn (str "al" p-counts))
    (prn (str "you" op-counts))

    (deb (if (or (some #(= 4 %) p-counts) (some #(= 4 %) op-counts))
           [:game-over player]
           (if (some #(= 3 %) p-counts) 
             [:win (keep-indexed is3 p-counts)]
             (if (some #(= 3 %) op-counts)
               [:block (keep-indexed is3 op-counts)]
               (if (some #(= 2 %) p-counts)
                 [:force (keep-indexed is2 p-counts)]
                 (if (some #(= 2 %) op-counts)
                   [:defend (keep-indexed is2 op-counts)]
                   (if (some #(= 1 %) p-counts)
                     [:enable-force (keep-indexed is1 p-counts)]
                     (if (some #(= 1 %) op-counts)
                       [:enable-defence (keep-indexed is1 op-counts)]
                       (if (not-every? #(or (= 0 %) (nil? %)) potential)
                         [:most-squares p-counts]
                         [:game-over nil])
                       )))))))))
)

(defn empty-point? [g p] 
  (if (and (not ((:as g) p)) (not ((:bs g) p)))
    p
    nil))

(defn best-points [m] 
  (let [max-val (apply max (vals m))]
    (map key (filter #(let [[k v] %] (= max-val v)) m))))

(defn best-tactical-move [g square-indices]
  "Choose a point to hit from the points inside the critical squares"
  ;; Fetch the squares to consider
  (->> square-indices
       deb
       (map #(if (nil? %) nil (nth (:squares g) %))) ;; convert to square point sets
       (mapcat vec) ;; then to a vector of points
       (filter #(empty-point? g %)) ;; remove already claimed points
       (reduce #(update %1 %2 inc) '{}) ;; count occurrences
       (deb)
       (best-points)
       (deb)
       (rand-nth)
       (deb))
)

(defn attack-3-4 [g square-indices] 
  (let [winning-square (nth (:squares g) (first square-indices))]
    (prn (str "win: " square-indices))
    (prn (str "winning-square: " winning-square))
    (prn (str  "move on " (some #(empty-point? g %) winning-square)))
    (best-tactical-move g square-indices)
    ))

(defn defend-3-4 [g square-indices] 
  (prn (str "block: " square-indices))
  (best-tactical-move g square-indices))

(defn attack-2-3 [g square-indices] 
  (prn (str "force: " square-indices))
  (best-tactical-move g square-indices))

(defn defend-2-3 [g square-indices] 
  (prn (str "defend: " square-indices))
  (best-tactical-move g square-indices))

(defn attack-1-2 [g square-indices] 
  (prn (str "enable-force: " square-indices))
  (best-tactical-move g square-indices))

(defn defend-1-2 [g square-indices] 
  (prn (str "enable-defence: " square-indices))
  (best-tactical-move g square-indices))

(defn best-0 [g player square-indices]
  "Find the point with the most squares"
  (best-tactical-move g square-indices))

(defn apply-tactics [g player]
  (let [[tactic square-indices] (get-tactic (game-potential g) player)]
    (condp = tactic 
      :win   (attack-3-4 g square-indices) ; 3->4
      :block (defend-3-4 g square-indices) ; stop 3->4
      :force (attack-2-3 g square-indices) ; 2->3
      :defend (defend-2-3 g square-indices) ; stop 2->3
      :enable-force (attack-1-2 g square-indices) ; best 1->2
      :enable-defence (defend-1-2 g square-indices) ; stop best 1->2
      :most-squares (best-0 g player square-indices) ; choose point on most squares
      :game-over (prn "Game Over" square-indices)
      )))

(defn fill-color [g p]
  (if ((:as g) p) 
    (:a player-colours)
    (if ((:bs g) p)
      (:b player-colours)
      (:none player-colours)))
  )

(defn dot-sep [n]
  (Math.floor (/ 300 n)))

(defn px [n len]
  (str (* len (dot-sep n)) "px"))

;;
;; reset the game initially
;;
(declare reset-game)

(defn up-tap [event]
  "grow the game by 1 unit up to a max-n square"
  (.stopPropagation event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n) max-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n
                         :squares (all-squares new-n)))))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (let [old-n (:n @game)
        new-n (if (> old-n min-n) (- old-n 1) min-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n
                         :squares (all-squares new-n)))
  ))

(defn claim-a-point [as point]
  (swap! game #(assoc % :player :b :as (conj as point))))

(defn claim-b-point [bs point]
  (swap! game #(assoc % :player :a :bs (conj bs point))))

(defn claim-point [as bs point player]
  (if (and (not (as point)) (not (bs point)))
    (if (= player :a)
      (claim-a-point as point)
      (claim-b-point bs point)
      )))

(defn computer-turn [g]
  (prn "play computer turn")
  (apply-tactics g :b))



(declare timeout)

#_(defn test-pp [g as point]
  (claim-a-point as point)
  (timeout 2000 #(prn @game)))

(defn single-player-point [g as bs point] 
  (when (not (game-over? g)) (claim-a-point as point))
  (when (not (game-over? @game))
    (timeout al-think-time #(->> @game
                                 (computer-turn)
                                 (claim-b-point bs))))
)

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do 
      (.stopPropagation event)
      (if (= (:players g) 2)
        (claim-point as bs p pl)
        (when (= pl :a)
          (single-player-point g as bs p))))))


(r/defc svg-dot < r/reactive [n x y fill]
  [:circle {
            :class "dot"
            :cx (gaps x) 
            :cy (gaps y)
            :r (units 8)
            :fill fill
            :stroke "#cceecc"
            :stroke-width (units  8)
            :id (str "[" x " " y "]")
            :key (str "[" x " " y "]")
            :on-click handle-tap
            :on-touch-end handle-tap
            }])

(r/defc svg-grid < r/reactive [g]
  [:section {:key "b3" :style {:height "60%"}}
   [:svg {:view-box (str "0 0 " bound-width " " bound-height) 
          :height "100%"
          :width "100%"
          :key "b3"}
    (let [n (:n g)] 
      [:g {:id "box" :transform (str "scale(" (* 1 (/ max-n n)) ")")}
       (for [x (range n)]
         (for [y (range n)]
           (svg-dot n x y (fill-color g [x y])) ))])]])

(r/defc debug-game < r/reactive [g]
  [:p {:key "b1"} (str (dissoc g :squares))]
)

(defn active [g player-count]
  (if (= player-count (:players g)) "active" "")
)

(defn one-player [event]
  (.stopPropagation event)
  (swap! game #(assoc % :players 1)))

(defn two-player [event]
  (.stopPropagation event)
  (swap! game #(assoc % :players 2)))

(defn reset-game 
  ([]
   #_(reset! game initial-state)
   (swap! game #(assoc % :squares (all-squares (:n @game))
                         :player :a
                         :as #{}
                         :bs #{})))
  ([event] 
   (.stopPropagation event) 
   (reset-game))
  ([event _]
   (reset-game event)))

(r/defc tool-bar < r/reactive [g]
  [:div
   [:div {:class "btn-group toolbar"}
    [:button {:type "button" :class "btn btn-warning" :key "bu1" :on-click down-tap :on-touch-end down-tap} 
     [:span {:class "fa fa-chevron-down"}]]
    [:button {:type "button" :class "btn btn-warning" :key "bu2" :on-click up-tap :on-touch-end up-tap} 
     [:span {:class "fa fa-chevron-up"}]]
    [:button {:type "button" :class (str "btn btn-default " (active g 1)) :key "bu4" :on-click one-player :on-touch-end one-player} "1 player"]
    [:button {:type "button" :class (str "btn btn-default " (active g 2)) :key "bu5" :on-click two-player :on-touch-end two-player} "2 player"]
    [:button {:type "button" :class "btn btn-danger" :key "bu3" :on-click reset-game :on-touch-end reset-game} 
     [:span {:class "fa fa-refresh"}]]
    ]
   ])

(defn get-status [g]
  (let [pa (= (:player g) :a)
        gover (game-over? g)]
    (if (game-drawn? g)
      :draw
      (if (= (:players g) 1)
        (if gover
          (if pa :al-win :you-win)
          (if pa :yours :als))
        (if gover
          (if pa :b-win :a-win)
          (if pa :as-turn :bs-turn))
        ))))

(defn get-message [status]
  (status messages))

(defn get-fill [status]
  ((status message-colours) player-colours))

(r/defc status-bar < r/reactive [g]
  (let [status (get-status g)]
    [:p {:class "status" :style {:background-color (get-fill status)} :key "b4"} (get-message status)]))

(r/defc board  < r/reactive []
  (let [g (r/react game)]
    [:section
     [:div {:class "full-width"}
      [:h1 "Square it!"]
      (tool-bar g)
      (status-bar g)]
     (svg-grid g)
     (debug-game g)
     ]
))


(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc)
)

(r/mount (board) (.getElementById js/document "game"))

(reset-game)

;;;;;;;;;;;;;; 
;;
;; timer i/o
;;
(defn timeout [ms f & xs]
  "Call f, optionally with arguments xs, after ms milliseconds"
  (js/setTimeout #(apply f xs) ms))



