(ns ^:figwheel-always square-it.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [cljs.pprint :refer (pprint)]
              [cljsjs.react]
              [db.squares]))

(enable-console-print!)

(prn "-- Restart --")

(defonce game (atom {:n 5
                     :player :a
                     :as #{}
                     :bs #{}
                     }))

(def unit 1.19)
(def gap 30)
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

(defn up-tap [event]
  (.stopPropagation event)
  (let [inc-lt (fn [n m] (if (< n m) (inc n) m))]
    (swap! game #(update % :n inc-lt 9)))
)

(defn down-tap [event]
  (.stopPropagation event)
  (let [decz (fn [n m] (if (> n m) (- n 1) m))]
    (swap! game #(update % :n decz 3)))
)

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do 
      (.stopPropagation event)
      (if (and (not (as p)) (not (bs p)))
        (if (= pl :a)
          (swap! game #(assoc % 
                         :player :b
                         :as (conj as p)))
          (swap! game #(assoc % 
                         :player :a
                         :bs (conj bs p)))
          ))
      (.log js/console (str p)))))

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
            }]
  )

(r/defc svg-grid < r/reactive [g]
  [:section {:key "b3" :style {:height "90vw"}}
   ;; NB increase view-box width to provide more height for toolbars etc.
   [:svg {:view-box "0 0 320 370" 
          :height "100%"
          :width "100%"
          :key "b3"}
    (let [n (:n g)] 
      [:g {:transform (str "scale(" (/ 9 n) ")")}
       (for [x (range n)]
         (for [y (range n)]
           (svg-dot n x y (fill-color g [x y])) ))])]])

(r/defc debug-game < r/reactive [g]
  [:p {:key "b1"} (str g)]
)

(r/defc tool-bar < r/reactive [g]
  [:div {:class "btn-group toolbar"}
   [:button {:type "button" :class "btn btn-warning" :key "bu1" :on-click down-tap :on-touch-end down-tap} 
    [:span {:class "fa fa-chevron-down"}]]
   [:button {:type "button" :class "btn btn-warning" :key "bu2" :on-click up-tap :on-touch-end up-tap} 
    [:span {:class "fa fa-chevron-up"}]]
   [:button {:type "button" :class "btn btn-default" :key "bu4"} "1 player"]
   [:button {:type "button" :class "btn btn-default active" :key "bu5"} "2 player"]
   [:button {:type "button" :class "btn btn-danger" :key "bu3"} 
    [:span {:class "fa fa-refresh"}]]
   ])

(r/defc status-bar < r/reactive [g]
  [:p {:key "b4"} "Status Bar"])

(r/defc board  < r/reactive []
  (let [g (r/react game)]
    [:section
     #_(debug-game g)
     [:div {:class "full-width"}
      (tool-bar g)]
     (svg-grid g)
     (status-bar g)
     ]
))



(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc)
)



(r/mount (board) (.getElementById js/document "game"))

