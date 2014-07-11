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
  [a b]
  (when-not (= a b)
    (let [ea (dom/by-id (name a))
          eb (dom/by-id (name b))
          dir (if (pos? (get config/dom-transitions [a b]))
                "future" "past")]
      (dom/set-class! ea dir)
      (dom/set-class! eb "present"))))

(defn transition-controllers
  [state route]
  (let [{:keys [bus ctrl-id]} @state
        {new-id :controller params :params} route]
    (swap!
     state merge
     {:route route
      :ctrl-id new-id
      :last-route-change (utils/now)})
    (async/publish
     bus (keyword (str "init-" (name new-id))) [state params])
    (go (<! (timeout config/controller-release-delay))
        (async/publish
         bus (keyword (str "release-" (name ctrl-id))) nil))))

(defn listen-route-change
  [bus]
  (let [ch (async/subscribe bus :route-changed)]
    (go
      (loop []
        (let [[_ [state new-route]] (<! ch)
              {curr-id :ctrl-id} @state
              new-id (:controller new-route)]
          (debug :new-route new-route)
          (transition-controllers state new-route)
          (transition-dom curr-id new-id)
          (recur))))))

(defn listen-dom
  [bus]
  (let [[resize] (dom/event-channel js/window "resize")]
    (go
      (loop []
        (let [_ (<! resize)]
          (async/publish bus :window-resize
                         [(.-innerWidth js/window)
                          (.-innerHeight js/window)])
          (recur))))))

(defn init-router
  [bus state routes default]
  (let [router (route/router
                routes default
                #(async/publish bus :route-changed [state %]))]
    (listen-route-change bus)
    (route/start-router! router)))

(defn init-modules
  [bus state]
  (listen-dom bus)
  (home/init bus)
  (selector/init bus)
  (editor/init bus)
  (init-router bus state config/routes config/default-route))

(defn ^:export start
  []
  (let [bus (async/pub-sub
             (fn [e]
               (debug :topic (first e))
               (first e)))
        state (atom {:bus bus
                     :ctrl-id :loader})
        satisfied? true]
    ;; TODO add feature check and redirect if not-supported
    (if satisfied?
      (init-modules bus state)
      (init-router
       bus state
       config/routes-unsupported config/default-route-unsupported))
    state))

(def app (start))
