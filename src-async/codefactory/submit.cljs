(ns codefactory.submit
  (:require-macros
   [cljs.core.async.macros :refer [go]])
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn submit-model
  [bus data]
  (io/request
   :uri     (config/api-route :submit-object)
   :method  :post
   :edn?    true
   :data    (config/inject-api-request-data data)
   :success (fn [status body]
              (debug :success-response status body)
              (async/publish bus :submit-model-success body))
   :error   (fn [status body]
              (debug :error-response status body)
              (async/publish bus :submit-model-fail body))))

(defn handle-init
  [bus]
  (let [ch (async/subscribe bus :init-submit)]
    (go
      (loop []
        (let [[_ [state]] (<! ch)
              form (aget (.-forms js/document) "submit-art-form")]
          (dom/set-attribs! (aget form "title") {:value ""})
          (dom/set-attribs! (aget form "author") {:value ""})
          (recur))))))

(defn handle-tree
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed]] (<! ch)]
          (swap! local assoc :tree tree :seed seed)
          (recur))))))

(defn handle-submit
  [bus local]
  (let [[ch] (async/event-channel (dom/by-id "bt-submit") "click")]
    (go
      (loop []
        (<! ch)
        (let [form   (aget (.-forms js/document) "submit-art-form")
              title  (.-value (aget form "title"))
              author (.-value (aget form "author"))
              {:keys [tree seed]} @local]
          (submit-model
           bus {:tree (pr-str tree)
                :seed seed
                :title title
                :author author})
          (recur))))))

(defn handle-success
  [bus]
  (let [ch (async/subscribe bus :submit-model-success)]
    (go
      (loop []
        (let [[_ data] (<! ch)
              {:keys [id]} (:body data)]
          (debug :success id)
          (route/set-route! "thanks" id)
          (recur))))))

(defn handle-cancel
  [bus local]
  (let [[ch] (async/event-channel (dom/by-id "submit-cancel") "click")]
    (go
      (loop []
        (<! ch)
        (route/set-route! "objects" "new" (:seed @local))
        (recur)))))

(defn init
  [bus]
  (let [local (atom {})]
    (handle-init    bus)
    (handle-tree    bus local)
    (handle-submit  bus local)
    (handle-success bus)
    (handle-cancel  bus local)))
