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
  (let [chan-i (async/subscribe bus :init-home)
        chan-r (async/subscribe bus :release-home)
        [click] (dom/event-channel (dom/by-id "home-continue") "click")]
    (go
      (loop []
        (let [[_ [state]] (<! chan-i)]
          (debug :init-home)
          (go
            (let [_ (<! click)]
              (route/set-route! "select")))
          (recur))))
    (go
      (loop []
        (let [[_ [state]] (<! chan-r)]
          (debug :release-home)
          (recur))))))