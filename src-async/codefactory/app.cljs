(ns codefactory.app
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.home :as home]
   [codefactory.editor :as editor]
   [codefactory.selector :as selector]
   [codefactory.submit :as submit]
   [codefactory.thanks :as thanks]
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
          dir (if (pos? (config/transition a b)) "future" "past")]
      (dom/set-class! ea dir)
      (dom/set-class! eb "present"))))

(defn transition-controllers
  [state {new-id :controller params :params :as route}]
  (let [{:keys [bus ctrl-id]} @state
        delay      (config/timeout :controller-release-delay)
        init-id    (keyword (str "init-" (name new-id)))
        release-id (keyword (str "release-" (name ctrl-id)))]
    (swap!
     state merge
     {:route             route
      :ctrl-id           new-id
      :last-route-change (utils/now)})
    (async/publish bus init-id [state params])
    (go
      (<! (timeout delay))
      (async/publish bus release-id nil))))

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
  (dom/add-listeners
   [[js/window "resize"
     (fn [_]
       (async/publish
        bus :window-resize
        [(.-innerWidth js/window) (.-innerHeight js/window)]))]]))

(defn init-router
  [bus state routes default-route-id]
  (let [router (route/router
                routes (routes default-route-id)
                #(async/publish bus :route-changed [state %]))]
    (listen-route-change bus)
    (route/start-router! router)))

(defn init-modules
  [bus state]
  (listen-dom bus)
  (let [{:keys [modules routes default-route]} config/app]
    (when (:home modules)     (home/init bus))
    (when (:selector modules) (selector/init bus))
    (when (:editor modules)   (editor/init bus))
    (when (:submit modules)   (submit/init bus))
    (when (:thanks modules)   (thanks/init bus))
    (init-router bus state routes default-route)))

(defn start
  []
  (let [bus    (async/pub-sub
                (fn [e] (debug :bus (first e)) (first e))
                ;;first
                )
        config (config/set-config! "__APP_CONFIG__")
        state  (atom {:bus bus :ctrl-id :loader})
        satisfied? true]
    ;; TODO add feature check and redirect if not-supported
    (if satisfied?
      (init-modules bus state)
      (init-router
       bus state
       (:routes-unsupported config)
       (:default-route-unsupported config)))
    state))

(.addEventListener js/window "load" start)
