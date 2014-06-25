(ns codefactory.core
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [codefactory.controllers.home :as home]
   [codefactory.controllers.editor :as edit]
   [thi.ng.cljs.app :as app :refer [handle-event emit]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.mobile :as mobile]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.log :refer [debug info warn]]
   [cljs.core.async :refer [>! <! chan put! close! timeout]]))

(enable-console-print!)

(def controllers
  {:loader nil
   :home home/instance
   :intro nil
   :editor edit/instance
   :gallery nil})

(defn transition-dom
  [a b]
  (when-not (= a b)
    (let [ea (dom/by-id (name a))
          eb (dom/by-id (name b))
          dir (if (pos? (get config/dom-transitions [a b]))
                "future" "past")]
      (dom/set-class! ea dir)
      (dom/set-class! eb "present"))))

(defmethod handle-event :route-changed
  [[_ new-route] state queue]
  (let [curr-id   (get-in @state [:route :controller])
        new-id    (:controller new-route)
        curr-ctrl (:controller @state)
        new-ctrl  (controllers new-id)]
    (debug :new-route new-route)
    (transition-dom curr-id new-id)
    (swap!
     state merge
     {:route new-route
      :controller new-ctrl
      :metrics {:last-route-change (utils/now)}})
    (proto/init
     new-ctrl
     {:state state
      :queue queue
      :params (:params new-route)})
    (when (and curr-ctrl (not= curr-id new-id))
      (js/setTimeout #(proto/release curr-ctrl) config/controller-release-delay))))

(defn start-router!
  [queue]
  (route/start-router!
   (route/router
    config/routes
    config/default-route
    queue)))

(def app nil)

(defn ^:export start
  []
  (let [state (atom
               {:controller (controllers :loader)
                :route {:controller :loader}})
        queue (chan)]
    (mobile/hide-address-bar)
    (app/event-dispatcher state queue)
    (start-router! queue)
    (set! app {:state state :queue queue})))

(.addEventListener js/window "load" start)
