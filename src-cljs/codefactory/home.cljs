(ns codefactory.home
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init-fullscreen-button
  []
  (let [tools (dom/query nil "#home .tools-extra")
        icons (:icons config/app)
        size (-> config/app :editor :toolbar-icon-size)]
    (common/icon-button
     tools nil size (-> icons :fullscreen :paths) nil
     (fn [] (dom/request-fullscreen))
     "fs-toggle")))

(defn init-button-bar
  []
  ;; TODO enable gallery button
  (dom/add-listeners
   [[(config/dom-component :home-continue) "click"
     #(route/set-route! "select")]]))

(defn show-credits
  [{:keys [title author date]}]
  (dom/set-style! (dom/query nil "#home .credits") #js {:visibility "visible"})
  (dom/set-text! (dom/by-id "credits-title") title)
  (dom/set-text! (dom/by-id "credits-author") author)
  (dom/set-text! (dom/by-id "credits-date") date))

(defn init
  [bus]
  (let [init (async/subscribe bus :init-home)]

    (init-fullscreen-button)
    (init-button-bar)

    (go
      (loop []
        (let [[_ [state]] (<! init)]
          (async/publish bus :broadcast-tree nil)
          (when-let [credits (:credits @state)]
            (show-credits credits)))
        (recur)))))
