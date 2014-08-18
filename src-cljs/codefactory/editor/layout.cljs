(ns codefactory.editor.layout
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [<! alts! timeout]]
   [codefactory.editor.tree :as tree]
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.rect :as r]
   [thi.ng.common.math.core :as m]))

(defn cell-size
  [total gap num]
  (if (pos? num)
    (/ (- total (* (dec num) gap)) num)
    total))

(defn cell-size-at
  [tree path width gap]
  (reduce
   (fn [width n]
     (->> path
          (take n)
          (tree/num-children-at tree)
          (cell-size width gap)))
   width (range (inc (count path)))))

(defn viewport-width
  []
  (let [{:keys [margin map-width]} (:editor config/app)]
    (mm/madd margin -3 (- (.-innerWidth js/window) map-width))))

(defn compute-viewport
  [local]
  (let [{:keys [scroll]} @local
        {:keys [height]} (:editor config/app)]
    [(- (:x scroll)) (:y scroll) (viewport-width) height]))

(defn scroll-offset
  [scroll el]
  (let [{:keys [height margin-bottom]} (:editor config/app)]
    (g/+ scroll
         (.-offsetLeft el)
         (mm/sub (.-innerHeight js/window) height margin-bottom))))

(defn map-focus-rect
  [[x y w h] viz-width viz-height map-width map-height]
  (let [x (m/map-interval x 0 viz-width  1 (- map-width 2))
        y (m/map-interval y 0 viz-height 1 (- map-height 2))
        w (m/map-interval w 0 viz-width  1 (- map-width 2))
        h (m/map-interval h 0 viz-height 1 (- map-height 2))]
    [x y w h]))

(defn compute-required-width
  [editor]
  (let [{:keys [max-nodes-path tree]} @editor
        {:keys [gap margin min-size]} (:editor config/app)
        width  (viewport-width)
        cw     (cell-size-at tree max-nodes-path width gap)]
    (if (< cw min-size)
      (loop [path     max-nodes-path
             num-sibs (tree/num-children-at tree max-nodes-path)
             width    min-size]
        (let [width'  (mm/madd width num-sibs gap (dec num-sibs))]
          (if (seq path)
            (let [path' (pop path)]
              (recur path' (tree/num-children-at tree path') width'))
            width')))
      width)))

;; new layout...

(defn init-node-weights
  [paths] (zipmap paths (repeat 0)))

(defn leaf-nodes
  [tree paths] (filter #(zero? (tree/num-children-at tree %)) paths))

(defn compute-branch-weights
  [acc tree path]
  (loop [acc acc, path path]
    (debug :path path)
    (let [acc (update-in acc [path] (fnil inc 0))]
      (if (seq path)
        (recur acc (pop path))
        acc))))

(defn compute-node-weights
  [tree paths]
  (reduce
   #(compute-branch-weights % tree %2)
   (init-node-weights paths)
   (leaf-nodes tree paths)))

(defn node-width-at
  [acc index path]
  (let [v (index path)
        p' (pop path)]
    (assoc acc path (* (/ v (index p')) (acc p')))))

(defn compute-node-sizes
  [index total-width]
  (reduce
   (fn [acc p] (node-width-at acc index p))
   {[] total-width}
   (drop 1 (sort (keys index)))))

(defn scale-nodes
  [index scale]
  (reduce-kv (fn [idx k v] (assoc idx k (* v scale))) index index))

(defn scale-nodes-to-min-size
  [index min-size]
  (let [min (reduce min (vals index))
        scale (/ min-size min)]
    (scale-nodes index scale)))
