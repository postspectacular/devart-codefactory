(ns codefactory.core
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [codefactory.controllers.home :as home]
   [codefactory.controllers.editor :as edit]
   [thi.ng.cljs.appstate :as app]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]))

(enable-console-print!)

(def controllers
  {:loader nil
   :home home/instance
   :intro nil
   :editor edit/instance
   :gallery nil})

(def state
  (app/make-app-state
   {:controller (controllers :loader)
    :route {:controller :loader}}))

(defn init-controllers
  []
  (app/listen-state-update!
   state :ctrl [:controller]
   (fn [_ old new]
     (prn :ctrl-change old new)
     (proto/init new state)
     (when (and old (not= old new))
       (js/setTimeout #(proto/release old) 900))))
  (.addEventListener
   (dom/by-id "arrow") "click"
   (fn [] (route/set-route! :edit "new")))
  (.addEventListener
   (dom/by-id "edit-cancel") "click"
   (fn [] (route/set-route! :home))))

(defn transition-controller
  [{ca :controller :as old} {cb :controller :as new}]
  (let [a (dom/by-id (name ca))
        b (dom/by-id (name cb))
        dir (if (pos? (get config/route-transitions [ca cb]))
              "future" "past")]
    (prn :cb cb (controllers cb))
    (swap! state assoc :controller (controllers cb))
    (dom/set-class! a dir)
    (dom/set-class! b "present")))

(defn route-changed
  [_ old new]
  (prn :route-changed new)
  (transition-controller old new))

(defn route-dispatcher
  [new-route]
  ;;(prn :new-route new-route)
  (swap!
   state merge
   {:route new-route
    :metrics {:last-route-change (utils/now)}}))

(defn start-router!
  []
  (app/listen-state-change!
   state :route [:route] route-changed)
  (route/start-router!
   (route/router
    config/routes
    config/default-route
    route-dispatcher)))

(defn ^:export start
  []
  (init-controllers)
  (start-router!))

(.addEventListener js/window "load" start)
