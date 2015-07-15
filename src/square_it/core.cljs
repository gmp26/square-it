(ns ^:figwheel-always square-it.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (union)]
              [cljs.pprint :refer (pprint)]
              [cljsjs.react]
              [db.squares :as sq]))

(enable-console-print!)

(prn "-- Restart --")

(def initial-state {:n 3
                    :player :a
                    :players 1
                    :as #{}
                    :bs #{}
                    :squares (sq/all-squares (:n initial-state))
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
               })

(def message-colours {:yours :a
                      :als   :b
                      :as-turn :a
                      :bs-turn :b
                      :you-win :a
                      :al-win :b
                      :a-win :a
                      :b-win :b
                      })

(def bound-width 320)
(def bound-height 320)
(def max-n 9)
(def min-n 3)
(def unit 1)
(def gap 36)
(defn units [x] (* x unit))
(defn gaps [x] (* (units (+ x 0.5)) gap))

(def player-colours {:a "rgb(0, 153, 255)"
                     :b "rgb(238, 68, 102)"
                     :none "rgb(220,220,220)"
                     })

(def player-class {:a "blue"
                   :b "red"
                   :none "grey"
                   })

(defn unpainted? [g point]
  (let [as (:as g)
        bs (:bs g)]
    (if (not (or (as point) (bs point))) true nil)))

(defn unclaimed? [g points-in-square]
  (every? #(unpainted? g %) points-in-square))

(defn empty-squares [g]
  (filter #(unclaimed? g %) (:squares g)))

(defn raw-corner-count [square player-point?]
  (->> square
       (map #(if (player-point? %) 1 0))
       (reduce +) ))

(defn square-potential [as bs square]
  (let [as-count (raw-corner-count square as)
        bs-count (raw-corner-count square bs)]
    (if (= 0 (* as-count bs-count))
      [as-count bs-count]
      nil
      )))

(defn squares-potential [as bs squares]
  (map #(square-potential as bs %) squares)
)

(defn game-potential [g]
  (map #(square-potential (:as g) (:bs g) %) (:squares g))
)

(defn game-potential-after [g extra-as extra-bs]
  (let [as' (union (:as g) extra-as) 
        bs' (union (:bs g) extra-bs)]
    squares-potential as' bs' (:squares g)))

(defn is4 [i n] (if (= 4 n) i nil))
(defn is3 [i n] (if (= 3 n) i nil))
(defn is2 [i n] (if (= 2 n) i nil))
(defn is1 [i n] (if (= 1 n) i nil))

(defn get-tactic [potential player]
  (let [px (if (= player :a) 0 1)
        opx (- 1 px)
        p-counts (map #(nth % px) potential)
        op-counts (map #(nth % opx) potential)]

    (prn p-counts)
    (prn op-counts)

    (if (or (some #(= 4 %) p-counts) (some #(= 4 %) op-counts))
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
                  [:most-squares potential])))))))
    )
)

(defn empty-point? [g p] 
  (if (and (not ((:as g) p)) (not ((:bs g) p)))
    p
    nil))

(defn attack-3-4 [g detail] 
  (let [winning-square (nth (:squares g) (first detail))]
    (prn (str "win: " detail))
    (prn (str "winning-square: " winning-square))
    (prn (str  "move on " (some #(empty-point? g %) winning-square)))))

(defn defend-3-4 [g detail] 
  (let [blocking-square (nth (:squares g) (first detail))]
    (prn (str "block: " detail))
    (prn (str "blocking-square: " blocking-square))
    (prn (str  "move on " (some #(empty-point? g %) blocking-square)))))

(defn attack-2-3 [g detail] 
  (prn (str "force: " detail)))

(defn defend-2-3 [g detail] 
  (prn (str "defend: " detail)))

(defn attack-1-2 [g detail] 
  (prn (str "enable-force: " detail)))

(defn defend-1-2 [g detail] 
  (prn (str "enable-defence: " detail)))

(defn best-0 [g detail] 
  (prn (str "most-squares: " detail)))

(defn apply-tactics [g player]
  (let [[tactic detail] (get-tactic (game-potential g) player)]
    (prn (str tactic))
    (condp = tactic 
      :win   (attack-3-4 g detail)  ; 3->4
      :block (defend-3-4 g detail) ; stop 3->4
      :force (attack-2-3 g detail) ; 2->3
      :defend (defend-2-3 g detail) ; stop 2->3
      :enable-force (attack-1-2 g detail) ; best 1->2
      :enable-defence (defend-1-2 g detail)    ; stop best 1->2
      :most-squares (best-0 g detail)      ; choose point on most squares 
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
(reset-game)

(defn up-tap [event]
  "grow the game by 1 unit up to a max-n square"
  (.stopPropagation event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n))]
    (swap! game #(assoc % 
                         :n new-n
                         :squares (sq/all-squares new-n)))))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (let [decz (fn [n m] (if (> n m) (- n 1) m))]
    (swap! game #(update % :n decz min-n))
    (reset-game event))
)

(defn claim-point [as bs point player]

  (if (and (not (as point)) (not (bs point)))
    (if (= player :a)
      (swap! game #(assoc % 
                     :player :b
                     :as (conj as point)))
      (swap! game #(assoc % 
                     :player :a
                     :bs (conj bs point)))
      )))

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do 
      (.stopPropagation event)
      (claim-point as bs p pl)
      )))

(r/defc svg-dot < r/reactive [n x y fill]
  [:circle {
            :class "dot"
            :cx (gaps x) 
            :cy (gaps y)
            :r (units 8)
            :fill fill
            :stroke "#eeeeee"
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
   (reset! game initial-state)
   (swap! game #(assoc % :squares (sq/all-squares (:n @game)))))
  ([event] 
   (.stopPropagation event) 
   (reset-game)))


(r/defc tool-bar < r/reactive [g]
  [:div {:class "btn-group toolbar"}
   [:button {:type "button" :class "btn btn-warning" :key "bu1" :on-click down-tap :on-touch-end down-tap} 
    [:span {:class "fa fa-chevron-down"}]]
   [:button {:type "button" :class "btn btn-warning" :key "bu2" :on-click up-tap :on-touch-end up-tap} 
    [:span {:class "fa fa-chevron-up"}]]
   [:button {:type "button" :class (str "btn btn-default " (active g 1)) :key "bu4" :on-click one-player :on-touch-end one-player} "1 player"]
   [:button {:type "button" :class (str "btn btn-default " (active g 2)) :key "bu5" :on-click two-player :on-touch-end two-player} "2 player"]
   [:button {:type "button" :class "btn btn-danger" :key "bu3" :on-click reset-game :on-touch-end reset-game} 
    [:span {:class "fa fa-refresh"}]]
   ])


#_(def messages {:yours "Your turn"
               :als   "Al's turn"
               :as-turn "Player A's turn"
               :bs-turn "Player B's turn"
               :you-win "Well done! You won"
               :al-win "Oops! You lost"
               :a-win "Player A won"
               :b-win "Player B won"
               })

(defn get-status [g]
  (if (= (:players g) 1)
    (if (= (:player g) :a) :yours :als)
    (if (= (:player g) :a) :as-turn :bs-turn)
    ))

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
     (debug-game g)
     [:div {:class "full-width"}
      (tool-bar g)
      (status-bar g)]
     (svg-grid g)
     ]
))


(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc)
)

(r/mount (board) (.getElementById js/document "game"))

