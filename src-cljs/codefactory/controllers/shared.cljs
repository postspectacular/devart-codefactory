(ns codefactory.controllers.shared
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.app :as app :refer [emit]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.rect :as r]))

(defn cancel-module
  [id] (fn [] (route/set-route! id)))

(def resize-window*
  (fn [state initial render]
    (let [{:keys [gl canvas]} initial]
      (fn []
        (let [[w h] (dom/size (dom/parent canvas))
              view-rect (r/rect 0 0 w h)]
          (set! (.-width canvas) w)
          (set! (.-height canvas) h)
          (app/merge-state
           state
           {:canvas-width w :canvas-height h
            :view-rect view-rect
            :proj (gl/perspective 45 view-rect 0.1 10)})
          (gl/set-viewport gl view-rect)
          (render state))))))

(defn svg-hover
  [color]
  (fn [e]
    (dom/set-style!
     (dom/query (.-target e) "svg g")
     #js {:stroke color})))
