(ns chess-960.core
  (:require [reagent.core :as reagent :refer [atom]]
            [chess-960.randompos :refer [rand-position]]))

(def position (atom (rand-position)))

(defn create-board []
  (let [wpos  (first @position)
        bpos  (mapv #(str % "b") wpos)
        wp    (vec (take 8 (repeat "P")))
        bp    (mapv #(str % "b") wp)
        empty (vec (take 8 (repeat nil)))]
    [bpos bp empty empty empty empty wp wpos]))

(def board (atom (create-board)))

(defn wich-cell? [piece [y x]]
  (let [color (cond (and (even? y) (even? x)) :td.cell-white
                    (and (even? y) (odd? x))  :td.cell-black
                    (and (odd? y) (even? x))  :td.cell-black
                    (and (odd? y) (odd? x))   :td.cell-white)]
    (case piece
      "K"  [color {:style {:background-image "url(img/wk.svg)"}}]
      "Q"  [color {:style {:background-image "url(img/wq.svg)"}}]
      "R"  [color {:style {:background-image "url(img/wr.svg)"}}]
      "N"  [color {:style {:background-image "url(img/wn.svg)"}}]
      "B"  [color {:style {:background-image "url(img/wb.svg)"}}]
      "P"  [color {:style {:background-image "url(img/wp.svg)"}}]
      "Kb" [color {:style {:background-image "url(img/bk.svg)"}}]
      "Qb" [color {:style {:background-image "url(img/bq.svg)"}}]
      "Rb" [color {:style {:background-image "url(img/br.svg)"}}]
      "Nb" [color {:style {:background-image "url(img/bn.svg)"}}]
      "Bb" [color {:style {:background-image "url(img/bb.svg)"}}]
      "Pb" [color {:style {:background-image "url(img/bp.svg)"}}]
           [color])))

(defn render-board []
  (let [board @board
        cells (map #(into [:tr] (map (fn [x h] (wich-cell? x [%2 h]))
                                     % (range 8)))
                   board (range 8))]
    (into [:table.stage] cells)))

(defn show []
  [:div {:style {:width "310"
                 :margin "auto"}}
   [:h2 "Who cares about openings?"]
   [:h3 {:style {:color "#232231"}}
    (str "Id : " (second @position))]
   [:div {:on-click #(do (reset! position (rand-position))
                         (swap! board create-board))}
    [render-board]]
   [:h4 {:style {:color "#3F3939"}}
    "Tuoch the board to generate new positions"]])

(defn run []
  (reagent/render [show]
                  (js/document.getElementById "app")))

(run)
