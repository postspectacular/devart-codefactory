(ns codefactory.editor.overview
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [<! alts! timeout]]
   [codefactory.editor.tree :as tree]
   [codefactory.editor.layout :as layout]
   [codefactory.config :as config]
   [codefactory.color :as col]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.common.math.core :as m]))

(def map-op-color
  (memoize
   (fn [op]
     (col/gray-offset-hex
      (config/operator-color op)
      (-> config/app :editor :map-color-offset)))))

(defn draw-map-branch
  [ctx tree path x y w h sel]
  (let [{:keys [op out] :as node} (tree/node-at tree path)
        nc (count out)
        wc (layout/cell-size w 1 nc)
        col (map-op-color (config/translate-mg-op (tree/node-operator node)))]
    (if (or (not sel) (= path sel))
      (do
        (set! (.-fillStyle ctx) col)
        (set! (.-strokeStyle ctx) "")
        (.fillRect ctx x (inc (- y h)) w (dec h)))
      (do
        (set! (.-fillStyle ctx) "")
        (set! (.-strokeStyle ctx) col)
        (.strokeRect ctx x (inc (- y h)) (max (dec w) 1) (dec h))))
    (if (pos? nc)
      (loop [i 0]
        (when (< i nc)
          (draw-map-branch ctx tree (conj path i) (mm/madd i wc i 1 x) (- y h) wc h sel)
          (recur (inc i)))))))

(defn draw-map-labels
  [ctx labels cx cy col font leading]
  (let [num (count labels)]
    (set! (.-fillStyle ctx) col)
    (set! (.-font ctx) font)
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (loop [i 0, y (int (/ num -2))]
      (when (< i num)
        (.fillText ctx (labels i) cx (mm/madd leading y cy))
        (recur (inc i) (inc y))))))

(defn regenerate
  [editor local]
  (let [{:keys [ctx width height]} @local
        {:keys [tree tree-depth selection]} @editor
        {:keys [map-bg map-label-col map-width map-height] :as econf} (:editor config/app)
        [vx vy vw vh :as vp] (layout/map-focus-rect
                              (layout/compute-viewport local)
                              width height map-width map-height)]
    (swap! local assoc :viewport vp)
    (set! (.-fillStyle ctx) map-bg)
    (set! (.-strokeStyle ctx) nil)
    (set! (.-lineWidth ctx) 1)
    (.fillRect ctx 0 0 map-width map-height)
    (if (< 1 tree-depth)
      (do
        (draw-map-branch ctx tree [] 0 map-height
                         map-width (/ map-height tree-depth)
                         selection)
        (set! (.-strokeStyle ctx) "yellow")
        (set! (.-lineWidth ctx) 2)
        (.strokeRect ctx vx vy vw vh))
      (let [{:keys [map-labels map-label-font map-label-size]} econf]
        (draw-map-labels ctx map-labels
                         (/ map-width 2) (/ map-height 2)
                         map-label-col map-label-font map-label-size)))))
