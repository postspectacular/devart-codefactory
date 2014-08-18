(ns codefactory.editor.toolbar
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :as cas :refer [>! <! alts! chan put! close! timeout]]
   [codefactory.config :as config]
   [codefactory.common :as common]
   [codefactory.editor.operators :as ops]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.webgl.animator :as anim]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2]]
   [thi.ng.common.math.core :as m]
   [clojure.string :as str]))

(defn preset-separator
  [parent [w h]]
  (let [el  (dom/create! "div" parent)
        svg (dom/create-ns!
             dom/svg-ns "svg" el
             {:width w :height h :viewBox "0 0 1 1"
              :preserveAspectRatio "none"})]
    (dom/create-ns! dom/svg-ns "path" svg {:d "M0.5,0 L0.5,1"})
    (dom/add-class! el "sep")
    [w]))

(defn preset-button
  [parent id op label icon-size width bus & [handler]]
  (let [[el :as spec] (common/icon-button
                       parent id icon-size
                       (-> config/app :operators op :paths)
                       (str/replace label " " "<br/>")
                       (or handler #(async/publish bus :op-triggered id)))]
    (-> el
        (dom/add-class! (ops/op-class op))
        (dom/add-class! "disabled"))
    [width spec]))

(defn init
  [bus tools]
  (let [{icon-size :toolbar-icon-size
         op-width  :toolbar-op-width
         sep-size  :toolbar-sep-size
         offset    :toolbar-margin-left} (:editor config/app)
         [width specs] (reduce
                        (fn [[total specs] [id {:keys [label node]}]]
                          (let [op (config/translate-mg-op (:op node))
                                [w spec] (if (= :sep id)
                                           (preset-separator tools sep-size)
                                           (preset-button
                                            tools id op label
                                            icon-size op-width bus))
                                total' (+ total w)]
                            (if spec
                              [total' (assoc specs id (conj spec total))]
                              [total' specs])))
                        [0 {}] (:op-presets config/app))]
    (dom/set-style! tools #js {:width width})
    (preset-button
     (dom/create! "div" (dom/by-id "tools-left"))
     :undo :undo "undo"
     icon-size op-width bus
     (fn [] (async/publish bus :undo-triggered nil)))
    (preset-button
     (dom/create! "div" (dom/by-id "tools-right"))
     :delete :delete "empty"
     icon-size op-width bus)
    {:width width :offset offset :curr-offset offset :specs specs}))

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

(defn center-preset
  [bus spec]
  (when spec
    (let [off (nth spec 3)
          w (.-innerWidth js/window)
          x (mm/sub w off (mm/madd w 0.5 (-> config/app :editor :toolbar-op-width) 0.5))]
      (async/publish bus :update-toolbar-pos x))))

(defn highlight-selected-preset
  [id specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        ((if (= id k) dom/add-class! dom/remove-class!) el "selected")
        (recur (next specs))))))

(defn disable-presets
  [specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        (dom/remove-class! el "selected")
        (dom/add-class! el "disabled")
        (recur (next specs)))))
  (dom/add-class! (dom/by-id "delete") "disabled"))

(defn enable-presets
  [specs]
  (loop [specs specs]
    (if specs
      (let [[k [el]] (first specs)]
        (dom/remove-class! el "disabled")
        (recur (next specs)))))
  (dom/remove-class! (dom/by-id "delete") "disabled"))
