(ns codefactory.gallery
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [codefactory.gallery.item :as item]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
   [hiccups.runtime :as h]
   [cljs.core.async :refer [<! timeout]]
   [clojure.string :as str]))

(defn load-page
  [bus page token]
  (let [q-conf (if token
                 (-> config/app :gallery :admin-query)
                 (-> config/app :gallery :query))
        offset (* page (:limit q-conf))]
    (dom/set-html!
     (config/dom-component :gallery-main)
     (common/loader-html "loading objects..."))
    (io/request
     :uri     (config/api-route :gallery)
     :params  (assoc q-conf :offset offset)
     :method  :get
     :edn?    true
     :success (fn [_ data]
                (async/publish bus :gallery-loaded (:body data)))
     :error   (fn [status data]
                (warn :response status data)))))

(defn toggle-page-button
  [id pred cls]
  (let [el   (config/dom-component id)
        cls' (if pred "disabled" cls)]
    (dom/set-attribs! (dom/query el "svg") {:class cls'})
    (-> el
        (dom/add-class!    cls')
        (dom/remove-class! (if pred cls "disabled")))))

(defn handle-page-change
  [bus local dir]
  (when-not (:loading? @local)
    (let [page (+ (:page @local) dir)]
      (when-not (neg? page)
        (swap!
         local assoc
         :loading?     true
         :page         page
         :prev-page    (:page @local)
         :prev-objects (:objects @local))
        (load-page bus page (:admin-token @local))))))

(defn init-button-bar
  [bus local]
  (dom/add-listeners
   [[(config/dom-component :gallery-prev) "click"
     #(handle-page-change bus local -1)]
    [(config/dom-component :gallery-next) "click"
     #(handle-page-change bus local 1)]]))

(defn build-gallery
  [objects bus token]
  (let [parent (config/dom-component :gallery-main)]
    (dom/clear! parent)
    (loop [objects (seq objects)]
      (when objects
        (item/gallery-item (first objects) parent bus token)
        (recur (next objects))))))

(defn handle-item-overlay
  [ch bus local]
  (let [parent (config/dom-component :gallery-main)]
    (go
      (while true
        (let [[_ [cmd item obj]] (<! ch)
              focused            (:focused @local)
              on?                (or (= :on cmd) (not= focused item))]
          (if focused
            (-> (dom/query focused ".obj-overlay")
                (dom/set-style! #js {:visibility "hidden"})
                (dom/remove-class! "fade-in")))
          (if on?
            (-> (dom/query item ".obj-overlay")
                (dom/set-style! #js {:visibility "visible"})
                (dom/add-class! "fade-in")))
          (swap! local assoc :focused (if on? item)))))))

(defn handle-refresh
  [ch bus local]
  (go
    (while true
      (let [[_ objects] (<! ch)
            {:keys [admin-token page prev-page]} @local]
        (swap! local assoc :loading? false)
        (if (seq objects)
          (do
            (swap! local assoc :objects objects)
            (build-gallery objects bus admin-token))
          (let [objects (:prev-objects @local)]
            (swap! local assoc :page prev-page)
            (build-gallery objects bus admin-token)))
        (toggle-page-button :gallery-prev (zero? page) "cancel")))))

(defn approve-item
  [bus local id]
  (io/request
   :uri      (config/api-route :approve-item id)
   :data     {:status "approved"}
   :sign-key (:admin-token @local)
   :method   :post
   :edn?     true
   :success  #(handle-page-change bus local 0)
   :error    (fn [status data]
               (warn :response status data)
               (handle-page-change bus local 0))))

(defn handle-approval
  [ch bus local]
  (go
    (while true
      (let [[_ id] (<! ch)]
        (approve-item bus local id)))))

(defn init
  [bus]
  (let [init    (async/subscribe bus :init-gallery)
        refresh (async/subscribe bus :gallery-loaded)
        focus   (async/subscribe bus :focus-gallery-item)
        approve (async/subscribe bus :approve-gallery-item)
        local   (atom {:focused nil :page 0})]

    (init-button-bar     bus local)
    (handle-refresh      refresh bus local)
    (handle-item-overlay focus bus local)
    (handle-approval     approve bus local)

    (go
      (while true
        (let [[_ [_ {:keys [token]}]] (<! init)]
          (swap!
           local assoc
           :focused      nil
           :page         0
           :prev-page    0
           :objects      []
           :prev-objects []
           :loading?     false
           :admin-token  token)

          (async/publish      bus :broadcast-tree nil)
          (toggle-page-button :gallery-prev true "cancel")
          (load-page          bus 0 token))))))
