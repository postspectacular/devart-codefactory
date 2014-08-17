(ns codefactory.editor.toolbar
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2]]
   [thi.ng.common.math.core :as m]))

(defn handle-toolbar-scroll
  [toolbar events bus state]
  (go
    (loop [gesture-state nil]
      (let [[[e data] ch] (alts! events)]
        (when e
          (recur
           (case e
             :drag-start (let [offset (get-in @state [:tools :offset] 0)
                               target (common/next-parent-id (:target data))]
                           (swap! state assoc-in [:tools :active?] true)
                           [offset (:p data) (vec2) (keyword target)])
             :drag-move  (if gesture-state
                           (let [[offset p _ target] gesture-state
                                 delta (g/- (:p data) p)
                                 offset' (mm/madd (:x delta) 2 offset)]
                             (async/publish bus :update-toolbar-pos offset')
                             [offset p delta target])
                           gesture-state)
             :gesture-end (when gesture-state
                            (let [[_ p delta target] gesture-state
                                  dist (g/mag delta)]
                              (when (and (:touch? data) (< dist 20))
                                ;;(debug :end-touch target dist)
                                (if (= target :undo)
                                  (async/publish bus :undo-triggered target)
                                  (async/publish bus :op-triggered target)))
                              (swap! state assoc-in [:tools :active?] false)
                              nil))
             nil)))))))

(defn handle-toolbar-update
  [bus state toolbar]
  (let [update (async/subscribe bus :update-toolbar)
        pos    (async/subscribe bus :update-toolbar-pos)]
    (go
      (loop []
        (<! update)
        (let [{:keys [offset curr-offset]} (:tools @state)
              curr-offset (m/mix curr-offset offset (-> config/app :editor :toolbar-speed))]
          (swap! state assoc-in [:tools :curr-offset] curr-offset)
          (dom/set-style! toolbar (clj->js {:marginLeft (->px curr-offset)}))
          (if (> (m/abs-diff curr-offset offset) 0.5)
            (when-not (:toolbar-frame @state)
              (swap!
               state assoc
               :toolbar-frame (anim/animframe-provider #(async/publish bus :update-toolbar nil)))))
          (swap! state dissoc :toolbar-frame))
        (recur)))

    (go
      (loop []
        (let [[_ offset] (<! pos)
              {max :toolbar-margin-left
               right :toolbar-margin-right} (:editor config/app)
               {:keys [width]} (:tools @state)
               min (- (.-innerWidth js/window) width right)
               offset (m/clamp offset min max)]
          (swap! state assoc-in [:tools :offset] offset)
          (when-not (:toolbar-frame @state)
            (async/publish bus :update-toolbar nil)))
        (recur)))))
