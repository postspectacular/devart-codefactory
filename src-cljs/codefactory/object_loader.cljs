(ns codefactory.object-loader
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [cljs.reader :refer [read-string]]
   [cljs.core.async :refer [<! timeout]]))

(defn toggle-error
  [state]
  (let [[on off] (if state
                   [:object-loader :object-error]
                   [:object-error :object-loader])]
    (dom/add-class!    (config/dom-component on)  "hidden")
    (dom/remove-class! (config/dom-component off) "hidden")))

(defn load-model
  [bus id]
  (io/request
   :uri     (config/api-route :get-object id)
   :method  :get
   :edn?    true
   :success (fn [_ {{:keys [tree seed id]} :body :as data}]
              (let [tree (if (string? tree) (read-string tree) tree)
                    seed (or seed "box")]
                (async/publish bus :broadcast-tree [tree (keyword seed) nil id])
                (js/setTimeout #(route/replace-route! "objects" "edit" seed) 1000)))
   :error   (fn [status body]
              (warn :response body)
              (toggle-error true))))

(defn init
  [bus]
  (let [init (async/subscribe bus :init-object-loader)]

    (dom/add-listeners
     [["#object-restart" "click" (fn [] (route/replace-route! "home"))]])
    
    (go
      (loop []
        (let [[_ [_ params]] (<! init)]
          (toggle-error false)
          (async/publish bus :broadcast-tree nil)
          (load-model bus (:id params))
          (recur))))))
