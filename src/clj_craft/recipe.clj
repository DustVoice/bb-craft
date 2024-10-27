(ns clj-craft.recipe
  (:gen-class)
  (:require [clojure.math :as math]
            [clojure.core.reducers :as r]))

(def recipes {"Dia" nil
              "Gold" nil
              "Iron" nil
              "Lithium Dust" nil
              "Osmium" nil
              "Quartz" nil
              "Redstone" nil
              "Sand" nil
              "Slime" nil
              "String" nil
              "Glass" {:ingr {"Sand" 1}}
              "Silicone" {:ingr {"Quartz" 1}}
              "QEI" {:ingr {"Iron" 3 "Quartz" 1} :yield 4}
              "Proc Binding" {:ingr {"String" 2 "Slime" 1} :yield 8}
              "Raw Basic Proc" {:ingr {"Iron" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Raw Impr Proc" {:ingr {"Gold" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Raw Adv Proc" {:ingr {"Dia" 1 "Redstone" 1 "Silicone" 1 "Proc Binding" 1}}
              "Basic Proc" {:ingr {"Raw Basic Proc" 1}}
              "Impr Proc" {:ingr {"Raw Impr Proc" 1}}
              "Adv Proc" {:ingr {"Raw Adv Proc" 1}}
              "1k Part" {:ingr {"Glass" 3 "Silicone" 4 "Redstone" 1 "QEI" 1}}
              "4k Part" {:ingr {"1k Part" 3 "Basic Proc" 4 "Redstone" 1 "QEI" 1}}
              "16k Part" {:ingr {"4k Part" 3 "Impr Proc" 4 "Redstone" 1 "QEI" 1}}
              "64k Part" {:ingr {"16k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "256k Part" {:ingr {"64k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "1024k Part" {:ingr {"256k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "4096k Part" {:ingr {"1024k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "16384k Part" {:ingr {"4096k Part" 3 "Adv Proc" 4 "Redstone" 1 "QEI" 1}}
              "Gravitational Modulating Unit" {:ingr {"Atomic Alloy" 2
                                                             "Nether Star" 1
                                                             "Ultimate Induction Provider" 2
                                                             "Module Base" 1
                                                             "Antimatter Pellet" 3}}
              "Atomic Alloy" {:ingr {"Reinforced Alloy" 1 "Refined Obsidian Dust" 1}}
              "Refined Obsidian Dust" {:ingr {"Obsidian Dust" 1 "Diamond Dust" 1}}
              "Reinforced Alloy" {:ingr {"Infused Alloy" 1 "Diamond Dust" 1}}
              "Infused Alloy" {:ingr {"Iron" 1 "Redstone" 1}}
              "Module Base" {:ingr {"Tin Ingot" 4 "Bronze Nugget" 4 "HDPE Sheet" 1} :yield 2}
              "Energy Tablet" {:ingr {"Redstone" 4 "Infused Alloy" 2 "Gold" 3}}
              "Ultimate Control Circuit" {:ingr {"Atomic Alloy" 2 "Elite Control Circuit" 1}}
              "Elite Control Circuit" {:ingr {"Reinforced Alloy" 2 "Advanced Control Circuit" 1}}
              "Advanced Control Circuit" {:ingr {"Infused Alloy" 2 "Basic Control Circuit" 1}}
              "Basic Control Circuit" {:ingr {"Osmium" 1 "Redstone" 1}}
              "Ultimate Energy Cube" {:ingr {"Atomic Alloy" 4 "Diamond" 2 "Energy Tablet" 2 "Elite Energy Cube" 1}}
              "Elite Energy Cube" {:ingr {"Reinforced Alloy" 4 "Gold" 2 "Energy Tablet" 2 "Advanced Energy Cube" 1}}
              "Advanced Energy Cube" {:ingr {"Infused Alloy" 4 "Osmium" 2 "Energy Tablet" 2 "Basic Energy Cube" 1}}
              "Basic Energy Cube" {:ingr {"Redstone" 4 "Iron" 2 "Energy Tablet" 2 "Steel Casing" 1}}
              "Steel Casing" {:ingr {"Steel" 4 "Glass" 4 "Osmium" 1}}
              "Ultimate Induction Provider" {:ingr {"Ultimate Control Circuit" 4 "Elite Induction Provider" 4 "Ultimate Energy Cube" 1}}
              "Elite Induction Provider" {:ingr {"Elite Control Circuit" 4 "Advanced Induction Provider" 4 "Elite Energy Cube" 1}}
              "Advanced Induction Provider" {:ingr {"Advanced Control Circuit" 4 "Basic Induction Provider" 4 "Advanced Energy Cube" 1}}
              "Basic Induction Provider" {:ingr {"Basic Control Circuit" 4 "Lithium Dust" 4 "Basic Energy Cube" 1}}
              })

(declare cook)

(defn itemcount
  "Returns a map of itemcounts {item count} needed to produce a singular instance of the given item."
  [item]
  (let [recipe (get recipes item)
        atom {item 1}]
    (cond
      (nil? recipe) atom
      :else (let [cooked (cook (:ingr recipe))
                  yield (:yield recipe)]
              (merge-with + atom (cond (nil? yield) cooked
                                       :else (update-vals cooked #(/ % yield))))))))

(defn cook
  "Returns a map of the total amount of items needed to fullfill a given item quantity list"
  [items]
  (->> items
       (map (fn [[name quantity]]
              (let [count (itemcount name)]
                (cond (= quantity 1) count
                      :else (update-vals count #(* % quantity))))))
       (apply merge-with +)))

(defn pcook
  "Returns a map of the total amount of items needed to fullfill a given item quantity list. Computation happens in parrallel"
  [items]
  (->> items
       (pmap (fn [[name quantity]]
              (let [count (itemcount name)]
                (cond (= quantity 1) count
                      :else (update-vals count #(* % quantity))))))
       (r/fold (partial merge-with +))))

(defn min-count
  "Returns the math/ceil of every itemcount"
  [itemlist]
  (update-vals itemlist #(int (math/ceil %))))

#_(println "Refined Storage: 16384k Part")
#_(doseq [[name quantity] (sort-by val > (min-count (cook {"16384k Part" 1})))] (println name ":" quantity))

#_(println "=========")

#_(println "Mekanism: Gravitational Modulating Unit")
#_(doseq [[name quantity] (sort-by val > (min-count (cook {"Gravitational Modulating Unit" 1})))] (println name ":" quantity))

(defn -main
  [& args]
  (time (sort-by val > (min-count (cook {"16384k Part" 1}))))
  (time (sort-by val > (min-count (pcook {"16384k Part" 1}))))
  (System/exit 0))
