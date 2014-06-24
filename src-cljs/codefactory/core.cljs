(ns codefactory.core
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   ;;[codefactory.controllers.home :as home]
   ;;[codefactory.controllers.editor :as edit]
   [thi.ng.cljs.app :as app :refer [dispatch-event emit]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.log :refer [debug info warn]]
   [cljs.core.async :refer [>! <! chan put! close! timeout]]))

(enable-console-print!)

(def controllers
  {:loader nil
   ;;:home home/instance
   :intro nil
   ;;:editor edit/instance
   :gallery nil})

(defn init-controllers
  [state queue]
  #_(app/listen-state-update!
     state :ctrl [:controller]
     (fn [_ old new]
       (debug :ctrl-change old new)
       (proto/init new state)
       (when (and old (not= old new))
         (js/setTimeout #(proto/release old) 900))))
  (.addEventListener
   (dom/by-id "arrow") "click"
   (fn [] (route/set-route! :edit "new")))
  (.addEventListener
   (dom/by-id "edit-cancel") "click"
   (fn [] (route/set-route! :home))))

#_(defn transition-controller
  [{ca :controller :as old} {cb :controller :as new}]
  (let [a (dom/by-id (name ca))
        b (dom/by-id (name cb))
        dir (if (pos? (get config/route-transitions [ca cb]))
              "future" "past")]
    (debug :cb cb (:controller @state))
    (dom/set-class! a dir)
    (dom/set-class! b "present")))

(defmethod dispatch-event :route-changed
  [[_ new-route] state queue]
  (let [curr-id   (get-in @state [:route :controller])
        new-id    (:controller new-route)
        curr-ctrl (:controller @state)
        new-ctrl  (controllers new-id)
        dir       (if (pos? (get config/route-transitions [curr-id new-id]))
                    "future" "past")]
    (dom/set-class! curr-id dir)
    (dom/set-class! new-id  "present")
    (debug :new-route new-route)
    (swap!
     state merge
     {:route new-route
      :controller new-ctrl
      :metrics {:last-route-change (utils/now)}})))

(defn start-router!
  [queue]
  (route/start-router!
   (route/router
    config/routes
    config/default-route
    queue)))

(defn ^:export start
  []
  (let [state (atom
               {:controller (controllers :loader)
                :route {:controller :loader}})
        queue (chan)]
    (app/event-dispatcher state queue)
    (init-controllers state queue)
    (start-router! queue)
    (def app {:state state :queue queue})))

(.addEventListener js/window "load" start)
