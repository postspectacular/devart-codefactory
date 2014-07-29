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
              (info :success-response status body)
              (async/publish bus :submit-model-success body))
   :error   (fn [status body]
              (warn :error-response status body)
              (async/publish bus :submit-model-fail body))))

(defn reset-form
  [id]
  (let [form (aget (.-forms js/document) id)]
    (set! (.-value (aget form "title")) "")
    (set! (.-value (aget form "author")) "")
    (-> (config/dom-component :submit-button)
        (dom/set-attribs! {:value "SUBMIT"})
        (dom/remove-attribs! ["disabled"])
        (dom/remove-class! "disabled"))))

(defn handle-init
  [bus local]
  (let [ch (async/subscribe bus :init-submit-form)]
    (go
      (loop []
        (let [[_ [state]] (<! ch)]
          (if (:tree @local)
            (reset-form "submit-art-form")
            (route/set-route! "home"))
          (recur))))))

(defn handle-tree
  [bus local]
  (let [ch (async/subscribe bus :broadcast-tree)]
    (go
      (loop []
        (let [[_ [tree seed history]] (<! ch)]
          (swap! local assoc :tree tree :seed seed :history history)
          (recur))))))

(defn handle-submit
  [bus local]
  (let [bt   (config/dom-component :submit-button)
        [ch] (async/event-channel bt "click")]
    (dom/add-listeners
     [[bt "click"
       (fn [e]
         (.preventDefault e)
         (let [form   (aget (.-forms js/document) "submit-art-form")
               title  (.-value (aget form "title"))
               author (.-value (aget form "author"))
               {:keys [tree seed]} @local]
           (-> bt
               (dom/set-attribs! {:value "PLEASE WAIT..." :disabled true})
               (dom/add-class! "disabled"))
           (submit-model
            bus {:tree (pr-str tree)
                 :seed seed
                 :title title
                 :author author})))]])))

(defn handle-cancel
  [bus local]
  (dom/add-listeners
   [[(config/dom-component :submit-cancel) "click"
     #(route/set-route! "objects" "edit" (:seed @local))]]))

(defn handle-success
  [bus]
  (let [ch (async/subscribe bus :submit-model-success)]
    (go
      (loop []
        (let [[_ data] (<! ch)
              {:keys [id short-uri]} (:body data)]
          (debug :success id short-uri)
          (route/set-route! "thanks")
          (recur))))))

(defn init
  [bus]
  (let [local (atom {})]
    (handle-init    bus local)
    (handle-tree    bus local)
    (handle-submit  bus local)
    (handle-cancel  bus local)
    (handle-success bus)))
