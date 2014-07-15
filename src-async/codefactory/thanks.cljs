(ns codefactory.thanks
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init
  [bus config]
  (let [init     (async/subscribe bus :init-submit-confirm)
        release  (async/subscribe bus :release-submit-confirm)
        [cancel] (dom/event-channel (dom/by-id "thanks-cancel") "click")
        active?  (atom false)]

    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (reset! active? true)
          (go
            (alts! [cancel (timeout 5000)])
            (when @active?
              (route/set-route! "home"))))
        (recur)))

    (go
      (loop []
        (<! release)
        (reset! active? false)
        (recur)))))
