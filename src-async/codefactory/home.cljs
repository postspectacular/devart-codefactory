(ns codefactory.home
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init
  [bus]
  (let [chan-i  (async/subscribe bus :init-home)
        chan-r  (async/subscribe bus :release-home)
        [click] (async/event-channel (config/dom-component :home-continue) "click")
        fs      (config/dom-component :fullscreen)]

    (dom/add-listeners
     [[fs "click"
       (fn []
         (dom/request-fullscreen)
         (dom/add-class! fs "hidden"))]])
    
    ;; TODO enable gallery button
    (go
      (loop []
        (let [[_ [state]] (<! chan-i)]
          (debug :init-home)
          (async/publish bus :broadcast-tree [nil nil])
          (go
            (let [_ (<! click)]
              (route/set-route! "select")))
          (recur))))
    (go
      (loop []
        (let [[_ [state]] (<! chan-r)]
          (debug :release-home)
          (recur))))))
