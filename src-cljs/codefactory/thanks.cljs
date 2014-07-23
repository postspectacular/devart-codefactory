(ns codefactory.thanks
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init
  [bus]
  (let [init     (async/subscribe bus :init-submit-confirm)
        release  (async/subscribe bus :release-submit-confirm)
        success  (async/subscribe bus :submit-model-success)
        [cancel] (async/event-channel (config/dom-component :thanks-cancel) "click")
        active?  (atom false)]

    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (reset! active? true)
          (go
            (alts! [cancel (timeout (config/timeout :thanks))])
            (when @active?
              (route/set-route! "home"))))
        (recur)))

    (go
      (loop []
        (<! release)
        (reset! active? false)
        (recur)))

    (go
      (loop []
        (let [[_ data] (<! success)
              {:keys [id]} (:body data)]
          (when (get-in config/app [:modules :workshop])
            (let [url (str "http://devartcodefactory.com/#/objects/" id)]
              (dom/set-html! (dom/by-id "art-url") (str "<a href=\"" url "\">" url "</a>"))))
          (recur))))))
