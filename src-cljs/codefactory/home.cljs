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

(defn init-buttons
  [bus]
  (let [tools (dom/query nil "#home .tools-extra")
        icons (:icons config/app)
        size (-> config/app :editor :toolbar-icon-size)]
    (common/icon-button
     tools nil size (-> icons :fullscreen :paths) nil
     (fn [] (dom/request-fullscreen))
     "fs-toggle")))

(defn show-credits
  [{:keys [title author date]}]
  (dom/set-style! (dom/query nil "#home .credits") #js {:visibility "visible"})
  (dom/set-text! (dom/by-id "credits-title") title)
  (dom/set-text! (dom/by-id "credits-author") author)
  (dom/set-text! (dom/by-id "credits-date") date))

(defn init
  [bus]
  (let [chan-i  (async/subscribe bus :init-home)
        chan-r  (async/subscribe bus :release-home)
        [click] (async/event-channel (config/dom-component :home-continue) "click")]

    (init-buttons bus)

    ;; TODO enable gallery button
    (go
      (loop []
        (let [[_ [state]] (<! chan-i)]
          (debug :init-home)
          (async/publish bus :broadcast-tree nil)
          (when-let [credits (-> config/app :home :credits)]
            (show-credits credits))
          (go
            (let [_ (<! click)]
              (route/set-route! "select")))
          (recur))))
    (go
      (loop []
        (let [[_ [state]] (<! chan-r)]
          (debug :release-home)
          (recur))))))
