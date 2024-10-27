(ns page-templates
  (:require [dali.io :as io])
  (:gen-class))

(def page {:width (- 100 5) :height (- 150 5)})

(def stroke-width 0.5)

(def spacing {:col 5 :row 5})

(def margin {:left 2.5 :right 0 :top 2.5 :bottom 2.5})

(def cols (let [available (- (:width page) (+ (:left margin) (:right margin)))]
            (range (+ 1 (/ available (:col spacing))))))
(def rows (let [available-height (- (:height page) (+ (:top margin) (:bottom margin)))]
            (range (+ 1 (/ available-height (:row spacing))))))

(def weekday {:cols (take (+ 7 1) cols)
              :rows rows})
(def description {:cols (drop (count (:cols weekday)) cols)
                  :rows (take-nth 2 rows)})
(def description-shadow {:cols (:cols description)
                         :rows (take-nth 2 (drop 1 rows))})

(defn mm [num] (str num "mm"))
(defn mm-xy [coord] (vec (map mm coord)))

(defn nth-xy [i xy] (case xy
                      :x (+ (:left margin) (* (:col spacing) i))
                      :y (+ (:top margin) (* (:row spacing) i))))

(defn grid-xy [col row] (mm-xy [(nth-xy col :x) (nth-xy row :y)]))


(def weekday-border-top [:line
                         (grid-xy (first (:cols weekday)) (first (:rows weekday)))
                         (grid-xy (last (:cols weekday)) (first (:rows weekday)))])
(def weekday-border-bot [:line
                         (grid-xy (first (:cols weekday)) (last (:rows weekday)))
                         (grid-xy (last (:cols weekday)) (last (:rows weekday)))])
(defn nth-weekday-line [col]
  [:line
   (grid-xy col (first (:rows weekday)))
   (grid-xy col (last (:rows weekday)))])
(def lines-weekday (conj (map nth-weekday-line (:cols weekday))
                         weekday-border-top
                         weekday-border-bot))

#_(def description-border-left [:line
                                (grid-xy (first (:cols description)) (first (:rows description)))
                                (grid-xy (first (:cols description)) (last (:rows description)))])
(defn nth-description-line [row]
  [:line
   (grid-xy (first (:cols description)) row)
   (grid-xy (last (:cols description)) row)])
(defn nth-description-shadow-line [row]
  [:line {:stroke-width (/ stroke-width 2)}
   (grid-xy (first (:cols description)) row)
   (grid-xy (last (:cols description)) row)])
(def lines-description (concat (map nth-description-line (:rows description))
                               (map nth-description-shadow-line (:rows description-shadow))))

(def lines-cutting-edge
  (list
   [:line {:stroke :grey :stroke-width "0.25mm"} (mm-xy [0 0]) (mm-xy [0 (:height page)])] ; TL -> BL
   [:line {:stroke :grey :stroke-width "0.25mm"} (mm-xy [0 0]) (mm-xy [(:width page) 0])] ; TL -> TR
   [:line {:stroke :grey :stroke-width "0.25mm"} (mm-xy [(:width page) 0]) (mm-xy [(:width page) (:height page)])] ; TR -> BR
   [:line {:stroke :grey :stroke-width "0.25mm"} (mm-xy [0 (:height page)]) (mm-xy [(:width page) (:height page)])])) ; BL -> BR

(def weekly-a6-lined
  [:dali/page {:width (mm (:width page)) :height (mm (:height page)) :stroke :black :stroke-width (mm stroke-width) :fill :none}
   lines-weekday
   lines-description
   lines-cutting-edge])

(def svgs {"weekly-a6-lined.svg" weekly-a6-lined})

(defn render-svgs
  "Render all supplied pages as SVGs"
  [pages]
  (doseq [[filename page] pages]
    (io/render-svg page filename)))

(render-svgs svgs)
