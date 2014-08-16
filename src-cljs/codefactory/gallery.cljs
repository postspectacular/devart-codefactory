(ns codefactory.gallery
  (:require-macros
   [cljs.core.async.macros :as asm :refer [go]])
  (:require
   [codefactory.config :as config]
   [codefactory.common :as common]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.utils :as utils :refer [->px]]
   [thi.ng.cljs.io :as io]
   [thi.ng.cljs.dom :as dom]
   [hiccups.runtime :as h]
   [cljs.core.async :as cas :refer [>! <! chan put! close! timeout]]))

(defn init-fullscreen-button
  []
  (let [tools (dom/query nil "#gallery .tools-extra")
        icons (:icons config/app)
        size (-> config/app :editor :toolbar-icon-size)]
    (common/icon-button
     tools nil size (-> icons :fullscreen :paths) nil
     dom/request-fullscreen
     "fs-toggle")))

(defn load-page
  [bus page]
  (let [q-conf (-> config/app :gallery :query)
        offset (* page (:limit q-conf))]
    (dom/set-html!
     (config/dom-component :gallery-main)
     (h/render-html
      [:div.loading
       [:p "loading objects..."]
       [:img {:src "/img/loading.gif" :alt "loading"}]]))
    (io/request
     :uri     (config/api-route :gallery)
     :params  (assoc q-conf :offset offset)
     :method  :get
     :edn?    true
     :success (fn [_ {:keys [body] :as data}]
                (async/publish bus :gallery-loaded body))
     :error   (fn [status body]
                (warn :response body)))))

(defn handle-page-change
  [bus local dir]
  (let [page (+ (:page @local) dir)]
    (debug :new-page page)
    (swap! local assoc :page page)
    (if (neg? page)
      (route/set-route! "home")
      (load-page bus page))))

(defn init-button-bar
  [bus local]
  (dom/add-listeners
   [[(config/dom-component :gallery-prev) "click"
     #(handle-page-change bus local -1)]
    [(config/dom-component :gallery-next) "click"
     #(handle-page-change bus local 1)]]))

(defn gallery-item
  [{:keys [id title author created preview-uri stl-uri] :as obj} parent bus]
  (let [img-url (str "/api/1.0/objects/" id "/preview")
        stl-url (str "/api/1.0/objects/" id "/stl")
        item (dom/create! "div" parent {:id (str "obj-" id)})
        {:keys [edit download]} (-> config/app :gallery :buttons)
        buttons (cond->
                 (list)
                 download (conj [:input.obj-download {:type "button" :value "download"}])
                 edit     (conj [:input.obj-edit {:type "button" :value "edit"}]))]
    (dom/set-html!
     item
     (h/render-html
      (list
       [:div.obj-preview
        [:div.obj-overlay.anim buttons]]
       [:div.credits
        [:span (str (.toUpperCase title) " by " (.toUpperCase author))]
        [:span (utils/format-date-time (js/Date. created))]])))
    (dom/set-style!
     (dom/query item ".obj-preview")
     (clj->js {:background-image (str "url(" img-url ")")}))
    (when (or edit download)
      (dom/add-listeners
       (cond->
        [[(dom/query item ".obj-preview") "mouseover"
          (fn [] (async/publish bus :focus-gallery-item [:on item obj]))]
         [(dom/query item ".obj-overlay") "mouseleave"
          (fn [] (async/publish bus :focus-gallery-item [:off item obj]))]
         [(dom/query item ".obj-preview") "touchstart"
          (fn [e] (.stopPropagation e) (async/publish bus :focus-gallery-item [:on item obj]))]
         [(dom/query item ".obj-overlay") "touchstart"
          (fn [e] (.stopPropagation e) (async/publish bus :focus-gallery-item [:off item obj]))]]
        edit     (conj [(dom/query item ".obj-edit") "click"
                        (fn [e] (.stopPropagation e) (route/set-route! "objects" id))])
        download (conj [(dom/query item ".obj-download") "click"
                        (fn [e] (.stopPropagation e) (route/set-location! stl-url))]))))))

(defn build-gallery
  [objects bus]
  (let [parent (config/dom-component :gallery-main)]
    (dom/set-html! parent "")
    (loop [objects (seq objects)]
      (when objects
        (gallery-item (first objects) parent bus)
        (recur (next objects))))))

(defn handle-item-overlay
  [ch bus local]
  (let [parent (config/dom-component :gallery-main)]
    (go
      (loop []
        (let [[_ [cmd item obj]] (<! ch)
              focused (:focused @local)
              on? (or (= :on cmd) (not= focused item))]
          (if focused
            (-> (dom/query focused ".obj-overlay")
                (dom/set-style! #js {:visibility "hidden"})
                (dom/remove-class! "fade-in")))
          (if on?
            (-> (dom/query item ".obj-overlay")
                (dom/set-style! #js {:visibility "visible"})
                (dom/add-class! "fade-in")))
          (swap! local assoc :focused (if on? item)))
        (recur)))))

(defn handle-refresh
  [ch bus local]
  (go
    (loop []
      (let [[_ objects] (<! ch)]
        (build-gallery objects bus))
      (recur))))

(defn init
  [bus]
  (let [init    (async/subscribe bus :init-gallery)
        refresh (async/subscribe bus :gallery-loaded)
        focus   (async/subscribe bus :focus-gallery-item)
        local   (atom {:focused nil :page 0})]

    (init-fullscreen-button)
    (init-button-bar bus local)
    (handle-refresh refresh bus local)
    (handle-item-overlay focus bus local)

    (go
      (loop []
        (let [[_ [state params]] (<! init)]
          (async/publish bus :broadcast-tree nil)
          (load-page bus 0))
        (recur)))))
