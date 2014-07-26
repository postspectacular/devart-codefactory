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
        local    (atom {:active? true :queue []})]

    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (swap! local assoc :active? true)
          (go
            (alts! [cancel (timeout (config/timeout :thanks))])
            (when (:active? @local)
              (route/set-route! "home"))))
        (recur)))

    (go
      (loop []
        (<! release)
        (swap! local assoc :active? false)
        (dom/add-class! (dom/by-id "art-url-wrapper") "hidden")
        (recur)))

    (go
      (loop []
        (let [[_ data] (<! success)
              {:keys [id]} (:body data)]
          (let [url (str "http://devartcodefactory.com/#/objects/" id)]
            ;;(dom/set-html! (dom/by-id "art-url") (str "<a href=\"" url "\">" url "</a>"))
            (dom/set-html! (dom/by-id "art-url") url))
          (dom/remove-class! (dom/by-id "art-url-wrapper") "hidden")
          (recur))))))
