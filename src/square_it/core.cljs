(ns ^:figwheel-always square-it.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (intersection)]
              [cljsjs.react]
              )
    )

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; square generation
;;
(defn square [n x y dx dy]
  "a square, bottom-left [x y], offset [dx dy] to bottom-right. Nil if any point lies outside game board."
  (let [x2 (+ x dx)
        y2 (+ y dy)
        x3 (- x2 dy)
        y3 (+ y2 dx)
        x4 (- x dy)
        y4 (+ y dx)]
    (if (and (< x2 n) (< y2 n) (>= x3 0) (< y3 n) (>= x4 0) (< y4 n))
      [[x y] [x2 y2] [x3 y3] [x4 y4]]))
)

;;; "100 Elapsed time: 1187 msecs" (28 -> 1101)
(defn all-squares [n]
  "generate all possible squares for a square board of size n"
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
               :als   "My turn"
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


(def player-colours {:a "rgb(20, 133, 255)"
                     :b "rgb(255, 68, 102)"
                     :none "rgb(0,0,0)"
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

(defn deb
  ([x]
   "print value as identity for debug use"
   (prn x) x)
  ([s x]
   (prn (str s x)) x))

;
; ****
;
(defn raw-corner-count [square player-points]
  "count corners of a square belonging to one player"
  (->> square
       (map #(if (player-points %) 1 0))
       (reduce +) ))

(defn square-potential [as bs square]
  "return [a b] ownership counts of a square. A doubly owned square returns nil"
  (let [as-count (raw-corner-count square as)
        bs-count (raw-corner-count square bs)]
    (if (= 0 (* as-count bs-count))
      [as-count bs-count]
      nil
      )))

(defn squares-potential [as bs squares]
  "return [a b] ownership counts of squares. Any doubly owned square returns a nil"
  (map #(square-potential as bs %) squares)
)

(defn game-potential [g]
  "return [a b] ownership counts of squares in a game. A doubly owned square returns nil"
  (squares-potential (:as g) (:bs g) (:squares g))
)

(defn game-drawn? [g]
  (every? nil? (game-potential g)))

(defn game-over? [g]
  (let [pot (game-potential g)]
    (or
     (some #(or (= 4 (first %)) (= 4 (second %))) pot)
     (game-drawn? g))))

(defn empty-point? [g p]
  (if (and (not ((:as g) p)) (not ((:bs g) p)))
    p
    nil))

(defn best-point-counts [m]
  (let [max-val (apply max (vals m))]
    [(map key (filter #(let [[k v] %] (= max-val v)) m)) max-val]))

(defn count-good-points [square-indices]
  (->> square-indices
       #_(deb "square-indices ")
       (map #(if (nil? %) nil (nth (:squares @game) %))) ;; convert to square point sets
       #_(deb "kept square points ")
       (mapcat vec) ;; then to a vector of points
       (filter #(empty-point? @game %)) ;; remove already claimed points
       (reduce #(update %1 %2 inc) '{}))) ;; count occurrences

(defn point-counts-of [m counts]
  (map key (filter #(let [[k v] %] (= m v)) counts)))

(defn best-m-level-counts [m player-counts]
  (let [is-m-n #(if (= m %2) %1 nil)]
    (->> player-counts
         #_(deb "player-counts: ")
         (map-indexed (fn [ix c] [ix c]))
         #_(deb "zipped: ")
         (point-counts-of m)
         #_(deb "point-counts-of: ")
         (count-good-points)
         #_(deb "count-good-points: ")
         (best-point-counts))))


(defn winning-square [g]
  "Winning square path data"
  (let [potential (game-potential @game)
        a-counts (point-counts-of
                  4 (map-indexed (fn [ix [ac bc]] [ix ac]) potential))
        b-counts (point-counts-of
                  4 (map-indexed (fn [ix [ac bc]] [ix bc]) potential))
        wp (nth (:squares @game) (first (flatten [a-counts b-counts])))
        p2s #(str (gaps (first %)) " " (gaps (second %)))
        sq2data #()
        ]
    (str "M " (p2s (first wp))
         " L " (p2s (second wp))
         " L " (p2s (nth wp 2))
         " L " (p2s (nth wp 3))
         " z")
    ))

(defn fork-m-level-check [m player p-counts op-counts]
  (let [have-m-p-counts (some #(= m %) p-counts)
        have-m-op-counts (some #(= m %) op-counts)]
    (cond

     (and have-m-p-counts (not have-m-op-counts))
     (let [[p pc :as best-p-counts] (best-m-level-counts m p-counts)]
       (prn "position force ")
       (rand-nth p))

     (and (not have-m-p-counts) have-m-op-counts)
     (let [[op opc :as best-op-counts] (best-m-level-counts m op-counts)]
       (prn "position defend ")
       (rand-nth op))

     (and have-m-p-counts have-m-op-counts)
     (let [[p pc :as best-p-counts] (best-m-level-counts m p-counts)
           [op opc :as best-op-counts] (best-m-level-counts m op-counts)
           common-points (intersection (set p) (set op))]
       (if (empty? common-points )
         (do
           (prn "empty")
           (if (>= pc opc)
             (do (prn (str "force: " best-p-counts)) (rand-nth p))
             (do (prn (str "defend: " best-op-counts)) (rand-nth op))))
         (do
           (prn (str "intersect: " common-points) )
           (rand-nth (vec common-points)))))

     :else
     #_nil
     (prn (str "no " m " level counts")))))

(defn get-ai-move [player]
  (let [potential (game-potential @game)
        p-counts (map second potential)
        op-counts (map first potential)]
    (or
     (fork-m-level-check 3 player p-counts op-counts)
     (fork-m-level-check 2 player p-counts op-counts)
     (fork-m-level-check 1 player p-counts op-counts))
))

(defn computer-turn [g]
  #_(prn "play computer turn")
  (get-ai-move :b)
)

(defn setup []
  (swap! game #(assoc %
                  :as #{[1 2] [2 1] [3 0]}
                  :bs #{[2 3] [2 2]})))

(defn fill-color [g p]
  (if ((:as g) p)
    (:a player-colours)
    (if ((:bs g) p)
      (:b player-colours)
      (:none player-colours))))

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
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n) max-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n
                         :squares (all-squares new-n)))))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (> old-n min-n) (- old-n 1) min-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n
                         :squares (all-squares new-n)))))

(defn claim-a-point [as point]
  (swap! game #(assoc % :player :b :as (conj as point))))

(defn claim-b-point [bs point]
  (swap! game #(assoc % :player :a :bs (conj bs point))))

(defn claim-point [as bs point player]
  (if (and (not (as point)) (not (bs point)))
    (if (= player :a)
      (claim-a-point as point)
      (claim-b-point bs point))))

(defn timeout [ms f & xs]
  "Call f, optionally with arguments xs, after ms milliseconds"
  (js/setTimeout #(apply f xs) ms))

(defn single-player-point [g as bs point]
  (do
    (when (not (or (game-over? g) (game-drawn? g)))
      (claim-a-point as point))
    (let [newg @game]
      (when (not (or (game-over? newg) (game-drawn? newg)))
        (timeout al-think-time #(->> newg
                                     (computer-turn)
                                     (claim-b-point bs)))))))

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do
      (.stopPropagation event)
      (.preventDefault event)
      (when (not (game-over? g))
        (if (= (:players g) 2)
          (claim-point as bs p pl)
          (when (= pl :a)
            (single-player-point g as bs p)))))))

(r/defc svg-dot < r/reactive [n x y fill]
  (let [p [x y]
        g (r/react game)
        the-class #(if (or ((:as g) p) ((:bs g) p)) "dot claimed" "dot")
        radius (if (or ((:as g) p) ((:bs g) p)) 8 6)
        ]
    [:circle {
              :class (the-class)
              :cx (gaps x)
              :cy (gaps y)
              :r (units radius)
              :fill fill
              :stroke "#cceecc"
              :stroke-width (units  8)
              :id (str "[" x " " y "]")
              :key (str "[" x " " y "]")
              :on-click handle-tap
              :on-touch-start handle-tap
              }]))

(declare get-status)
(declare get-fill)

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
           (svg-dot n x y (fill-color g [x y])) ))
       (when (and (game-over? g) (not (game-drawn? g)))
         [:g
          [:path {
                  :d (winning-square g)
                  :fill "none" ;;(get-fill (get-status g))
                  :opacity 0.5
                  :stroke (get-fill (get-status g))
                  :stroke-width "3"
                  }]
          ]
         )
       ])]])

(r/defc debug-game < r/reactive [g]
  [:p {:key "b1"} (str (dissoc g :squares))])

(defn active [g player-count]
  (if (= player-count (:players g)) "active" ""))

(defn one-player [event]
  (.stopPropagation event)
  (.preventDefault event)
  (swap! game #(assoc % :players 1 :player :a :as #{} :bs #{})))

(defn two-player [event]
  (.stopPropagation event)
  (.preventDefault event)
  (swap! game #(assoc % :players 2 :player :a :as #{} :bs #{})))

(defn reset-game
  ([]
   (swap! game #(assoc % :squares (all-squares (:n @game))
                         :player :a
                         :as #{}
                         :bs #{})))
  ([event]
   (.stopPropagation event)
   (.preventDefault event)
   (reset-game))
  ([event _]
   (reset-game event)))

(defn start-game []
  (if (empty? (:squares @game))
    (reset-game)))

(r/defc tool-bar < r/reactive [g]
  [:div
   [:div {:class "btn-group toolbar"}
    [:button {:type "button" :class "btn btn-warning" :key "bu1" :on-click down-tap :on-touch-start down-tap}
     [:span {:class "fa fa-chevron-down"}]]
    [:button {:type "button" :class "btn btn-warning" :key "bu2" :on-click up-tap :on-touch-start up-tap}
     [:span {:class "fa fa-chevron-up"}]]
    [:button {:type "button" :class (str "btn btn-default " (active g 1)) :key "bu4" :on-click one-player :on-touch-start one-player} "1 player"]
    [:button {:type "button" :class (str "btn btn-default " (active g 2)) :key "bu5" :on-click two-player :on-touch-start two-player} "2 player"]
    [:button {:type "button" :class "btn btn-danger" :key "bu3" :on-click reset-game :on-touch-start reset-game}
     [:span {:class "fa fa-refresh"}]]]])

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
          (if pa :as-turn :bs-turn))))))

(defn get-message [status]
  (status messages))

(defn get-fill [status]
  ((status message-colours) player-colours))

(r/defc status-bar < r/reactive [g]
  (let [status (get-status g)]
    [:p {:class "status" :style {:background-color (get-fill status)} :key "b4"} (get-message status)]))

(r/defc rules []
  [:p {:style {
               :text-align "center"
               :font-size 24
               :color "#888"
               }}
   "Claim all 4 corners of a square to win"])

(defn random-dark-colour []
  (let [dark #(+ 100 (rand-int 70))]
    (str "rgb(" (dark) "," (dark) "," (dark) ")")))

(def dark-rgb (random-dark-colour))

(r/defc goal [g]
  (let [n (:n g)]
    [:p {:style {
                 :text-align "center"
                 :font-size 24
                 :color "#fff"
                 :padding "5px"
                 :background-color dark-rgb
                 }}
     (condp = n
         3 "Can blue lose?"
         4 "Can blue lose in 4 moves?"
         5 "Can blue always win?"
         6 "Can blue force a draw?"
         7 "Can blue always lose?"
         8 "Can blue always win?"
         9 "Can blue always win on an infinite board?"
         )
     ]))

(r/defc board  < r/reactive []
  (let [g (r/react game)]
    [:section
     [:div {:class "full-width"}
      [:h1 "Square it!"]
      (tool-bar g)
      (status-bar g)]
     (svg-grid g)
     (rules)
     (goal g)
     #_(debug-game g)]))

(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))

(r/mount (board) (.getElementById js/document "game"))

(start-game)
