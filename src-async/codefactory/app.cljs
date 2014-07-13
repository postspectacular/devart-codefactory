(ns codefactory.app
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.home :as home]
   [codefactory.editor :as editor]
   [codefactory.selector :as selector]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [goog.events :as events]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn transition-dom
  [transitions a b]
  (when-not (= a b)
    (let [ea (dom/by-id (name a))
          eb (dom/by-id (name b))
          dir (if (pos? (get transitions [a b]))
                "future" "past")]
      (dom/set-class! ea dir)
      (dom/set-class! eb "present"))))

(defn transition-controllers
  [state route]
  (let [{:keys [bus ctrl-id config]} @state
        {new-id :controller params :params} route
        delay (get-in config [:timeouts :controller-release-delay])]
    (swap!
     state merge
     {:route route
      :ctrl-id new-id
      :last-route-change (utils/now)})
    (async/publish bus (keyword (str "init-" (name new-id))) [state params])
    (go (<! (timeout delay))
        (async/publish bus (keyword (str "release-" (name ctrl-id))) nil))))

(defn listen-route-change
  [bus]
  (let [ch (async/subscribe bus :route-changed)]
    (go
      (loop []
        (let [[_ [state new-route]] (<! ch)
              {curr-id :ctrl-id config :config} @state
              new-id (:controller new-route)]
          (debug :new-route new-route)
          (transition-controllers state new-route)
          (transition-dom (:dom-transitions config) curr-id new-id)
          (recur))))))

(defn listen-dom
  [bus]
  (let [listeners [[js/window "resize"
                     (fn [_] (async/publish bus :window-resize
                                           [(.-innerWidth js/window)
                                            (.-innerHeight js/window)]))]]]
    (dom/add-listeners listeners)))

(defn init-router
  [bus state routes default-route-id]
  (let [router (route/router
                routes (routes default-route-id)
                #(async/publish bus :route-changed [state %]))]
    (listen-route-change bus)
    (route/start-router! router)))

(defn init-modules
  [bus state config]
  (listen-dom bus)
  (home/init bus config)
  (selector/init bus config)
  (editor/init bus config)
  (init-router bus state (:routes config) (:default-route config)))

(defn ^:export start
  []
  (let [bus    (async/pub-sub
                (fn [e]
                  ;;(debug :topic (first e))
                  (first e)))
        config (js/eval (aget js/window "__APP_CONFIG__"))
        state  (atom {:bus bus
                      :ctrl-id :loader
                      :config config})
        satisfied? true]
    ;; TODO add feature check and redirect if not-supported
    (if satisfied?
      (init-modules bus state config)
      (init-router bus state
                   (:routes-unsupported config)
                   (:default-route-unsupported config)))
    state))

(.addEventListener js/window "load" start)
