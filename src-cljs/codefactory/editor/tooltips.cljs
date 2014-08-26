(ns codefactory.editor.tooltips
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [thi.ng.macromath.core :as mm])
  (:require
   [cljs.core.async :refer [<! alts! timeout]]
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.gestures :as gest]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2]]
   [thi.ng.common.math.core :as m]))

(def tooltip-element (memoize #(dom/by-id (str (name %) "-tip"))))

(defn add-tooltip-buttons
  [el bus state]
  (when (:intro-active? @state)
    (let [div (dom/create! "div" el)
          skip (dom/create! "input" div {:type "button" :value "skip"})
          next (dom/create! "input" div {:type "button" :value "next"})]
      (dom/add-listeners
       [[skip "click" #(async/publish bus :intro-done nil)]
        [next "click" #(async/publish bus :intro-next nil)]]))))

(defn handle-tooltip-display
  [bus state]
  (let [show     (async/subscribe bus :show-tooltip)
        hide     (async/subscribe bus :hide-tooltip)
        tooltips (-> config/app :editor :tooltips)
        tip-body (memoize #(dom/query % ".tooltip-content"))]
    (go
      (loop []
        (let [[_ [el id]] (<! show)
              tip (tooltip-element id)
              {:keys [offset intro-offset content auto?]} (tooltips id)
              body (tip-body tip)
              intro? (:intro-active? @state)
              offset (if intro? (or intro-offset offset) offset)
              [x y] (g/+ (vec2 (dom/offset el))
                         (if (fn? offset) (offset el) offset))]
          (dom/set-html! body content)
          (add-tooltip-buttons body bus state)
          (-> tip
              (dom/set-style! {:display "block" :left (->px x) :top (->px y)})
              (dom/remove-class! "hidden"))
          (when auto?
            (js/setTimeout
             #(async/publish bus :hide-tooltip id)
             (config/timeout :tooltip)))
          (recur))))
    (go
      (loop []
        (let [[_ id] (<! hide)]
          (dom/set-style! (tooltip-element id) {:display "none"})
          (recur))))))

(defn handle-tooltips
  [bus state]
  (let [tooltips (-> config/app :editor :tooltips)
        tips     (->> tooltips
                      (filter (comp :user? val))
                      keys
                      (mapv #(-> % name dom/by-id (dom/query "svg"))))
        channels (fn [ev] (set (mapv #(first (async/event-channel % ev)) tips)))
        on       (channels "mouseenter")
        off      (channels "mouseleave")
        touch    (channels "touchstart")
        all      (vec (concat on off touch))]
    (go
      (loop [tip-state {}]
        (let [[e ch] (alts! all)
              el (.-target e)
              id (-> el dom/parent (dom/get-attribs ["id"]) first)
              kid (keyword id)
              show? (or (on ch) (and (touch ch) (not (tip-state kid))))]
          (when (and id (not (:intro-active? @state)))
            (if show?
              (async/publish bus :show-tooltip [el kid])
              (async/publish bus :hide-tooltip kid)))
          (async/publish bus :user-action nil)
          (recur (assoc tip-state kid show?)))))))

(defn hide-tooltips
  []
  (->> (-> config/app :editor :tooltips)
       keys
       (map #(-> % tooltip-element (dom/add-class! "hidden")))
       (dorun)))

(defn handle-intro
  [bus state]
  (let [next (async/subscribe bus :intro-next)
        done (async/subscribe bus :intro-done)
        tips (-> config/app :editor :intro)]
    (go
      (loop []
        (<! next)
        (let [id  (:intro-id @state)
              id' (inc id)]
          (when (>= id 0)
            (async/publish bus :hide-tooltip (tips id)))
          (if (< id' (count tips))
            (let [kid (tips id')
                  el  (if-not (= :edit-canvas kid)
                        (-> kid name dom/by-id (dom/query "svg"))
                        (-> kid name dom/by-id))]
              (swap! state assoc-in [:intro-id] id')
              (async/publish bus :show-tooltip [el kid]))
            (async/publish bus :intro-done nil)))
        (async/publish bus :user-action nil)
        (recur)))

    (go
      (loop []
        (<! done)
        (let [id  (:intro-id @state)]
          (when (>= id 0)
            (async/publish bus :hide-tooltip (tips id)))
          (swap! state assoc :intro-active? false)
          (async/publish bus :regenerate-scene nil))
        (recur)))))
