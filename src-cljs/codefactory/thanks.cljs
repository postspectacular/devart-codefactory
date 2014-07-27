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
        local    (atom {:active? true})]

    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (if (:url @local)
            (do
              (swap! local assoc :active? true)
              (go
                (alts! [cancel (timeout (config/timeout :thanks))])
                (when (:active? @local)
                  (route/set-route! "home"))))
            (route/set-route! "home")))
        (recur)))

    (go
      (loop []
        (<! release)
        (swap! local assoc :active? false)
        (dom/add-class! (config/dom-component :thanks-wrapper) "hidden")
        (recur)))

    (go
      (loop []
        (let [[_ data] (<! success)
              {:keys [id]} (:body data)
              loc (.-location js/window)
              url (str (.-protocol loc) "//" (.-host loc) (.-pathname loc)
                       "#/objects/" id)
              el (config/dom-component :object-url)]
          (swap! local assoc :url url)
          (if (-> config/app :thanks :link-clickable?)
            (dom/set-html! el (str "<a href=\"" url "\">" url "</a>"))
            (dom/set-html! el url))
          (dom/remove-class! (config/dom-component :thanks-wrapper) "hidden")
          (recur))))))
