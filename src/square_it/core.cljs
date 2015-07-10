(ns ^:figwheel-always square-it.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [cljs.pprint :refer (pprint)]
              [cljsjs.react]
              [db.squares]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload
(defonce game (atom {:n 9
                     :player :a
                     :as #{}
                     :bs #{}
                     }))



(defn dot-sep [n]
  (Math.floor (/ 300 n)))

(defn px [n len]
  (str (* len (dot-sep n)) "px"))

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do 
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

(r/defc dot < r/reactive [n x y fill]
  [:div.dot
   {:style {:top (px n x) 
            :left (px n y)
            :background-color fill
            }
    :id (str "[" x " " y "]")
    :on-click handle-tap
    }]
  )

(defn fill-color [as bs p]
  (do
    (if (as p) 
      "#09f"
      (if (bs p)
        "#e46"
        "none"))
    ))

(r/defc grid < r/reactive [g]
  [:div.grid
   (let [n (:n g)
         as (:as g)
         bs (:bs g)] 
     (for [x (range n)]
       (for [y (range n)]
         (dot n x y (fill-color as bs [x y])))))
   ]
)

(r/defc debug-game < r/reactive [g]
  [:p (str g)]
)

(r/defc board  < r/reactive []
  (let [g (r/react game)]
    [:section#board 
     (debug-game g)
     (grid g)
     ]
))

(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc)
)



(r/mount (board) (.getElementById js/document "game"))

