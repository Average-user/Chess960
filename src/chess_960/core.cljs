(ns chess-960.core
  (:require [reagent.core :as reagent :refer [atom]]
            [chess-960.randompos :refer [positions]]))

(def ca (atom 0))
(def cb (atom 0))
(def cc (atom 0))

(defn position [i]
  (get positions (dec i)))

(defn create-board []
  (let [wpos' (first (position (+ @cc (* @cb 10) (* @ca 100))))
        wpos  (if (nil? wpos') (first (get positions 159)) wpos')
        bpos  (mapv #(str % "b") wpos)
        wp    (vec (repeat 8 "P"))
        bp    (mapv #(str % "b") wp)
        empty (vec (repeat 8 nil))]
    [bpos bp empty empty empty empty wp wpos]))

(def board (atom (create-board)))

(defn gen-random []
  (vec (first (nth (iterate (fn [[xs x]] [(cons (mod x 10) xs) (quot x 10)])
                       [[] (inc (rand-int 960))])
                   3))))

(defn reload-state [b]
  (if b
    (let [[a b c] (gen-random)]
      (do (reset! ca a)
          (reset! cb b)
          (reset! cc c)
          (swap! board create-board)))
    (swap! board create-board)))

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
  (let [cells (map #(into [:tr] (map (fn [x h] (wich-cell? x [%2 h]))
                                     % (range 8)))
                   @board (range 8))]
    (into [:table] cells)))

(defn board-by-nth [i]
  (first (get positions (dec i))))

(defn increase [value]
  [:input.counter
   {:type "button"
    :value @value
    :on-click #(do (swap! value (fn [x] (mod (inc x) 10)))
                   (reload-state false))}])

(defn main-page []
  [:center.main
   [:h2.title "Don't care about openings?"]
   [:center {:style {:margin "1px"}} [increase ca] [increase cb] [increase cc]]
   [:div {:on-click #(reload-state true)} (render-board)]
   [:h4.soft "Touch the board to generate new positions"]
   [:center
    [:a.icon {:href "https://github.com/Average-user/Chess960"
              :style {:content "url(img/github.png)"
                      :width   "32"
                      :height  "32"}}]]])

(defn run []
  (reagent/render [main-page]
                  (js/document.getElementById "app")))

(run)
