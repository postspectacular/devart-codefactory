(ns thi.ng.cljs.gestures
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec2]]
   [cljs.core.async :refer [chan put! close!]]))

(defn touch-gesture*
  [suffix]
  (let [drag (keyword (str "drag-" suffix))
        dual (keyword (str "dual-" suffix))]
    (fn [ch]
      (fn [e]
        (let [touches (.-touches e)]
          (.preventDefault e)
          (case (.-length touches)
            1 (let [t (aget touches 0)]
                (put! ch [drag {:p (vec2 (.-clientX t) (.-clientY t))
                                :touch? true
                                :target (.-target e)}]))
            2 (let [t1 (aget touches 0)
                    t2 (aget touches 1)
                    tp1 (vec2 (.-clientX t1) (.-clientY t1))
                    tp2 (vec2 (.-clientX t2) (.-clientY t2))
                    dist (g/dist tp1 tp2)]
                (put! ch [dual {:p tp1 :q tp2 :dist dist
                                :touch? true
                                :target (.-target e)}]))
            nil))))))

(def touch-gesture-start (touch-gesture* "start"))
(def touch-gesture-move (touch-gesture* "move"))

(defn gesture-end
  [ch]
  (fn [e]
    (put! ch [:gesture-end {:touch? (not (nil? (.-touches e)))
                            :target (.-target e)}])))

(defn mouse-gesture*
  [suffix]
  (let [id (keyword (str "drag-" suffix))]
    (fn [ch]
      (fn [e]
        (put! ch [id {:p (vec2 (.-clientX e) (.-clientY e))
                      :target (.-target e)}])))))

(def mouse-gesture-start (mouse-gesture* "start"))
(def mouse-gesture-move (mouse-gesture* "move"))

(defn mousewheel-proxy
  [ch]
  (fn [e]
    (put! ch [:mouse-wheel {:delta (or (aget e "deltaY") (aget e "wheelDeltaY") 0)}])))
