(ns codefactory.gallery.item
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
   [hiccups.runtime :as h]))

(defn item-listener
  [item q evt handler]
  [(dom/query item q) evt handler])

(defn gallery-item
  [{:keys [id title author created seed parent-id] :as obj}
   {:keys [edit info download approve]}
   parent bus & [attribs credits]]
  (let [edit    (and edit (config/editable-seed? seed))
        info    (and info (not approve))
        img-url (common/item-asset-url obj :preview)
        stl-url (common/item-asset-url obj :stl)
        item    (dom/create! "div" parent (merge attribs {:id (str "obj-" id)}))
        buttons (cond->
                 (list)
                 download (conj [:input.obj-download {:type "button" :value "download 3d"}])
                 edit     (conj [:input.obj-edit {:type "button" :value "edit"}])
                 info     (conj [:input.obj-info {:type "button" :value "details"}])
                 approve  (conj [:input.obj-approve {:type "button" :value "approve"}]))]
    (-> item
        (dom/set-html!
         (h/render-html
          (list
           [:div.obj-preview
            [:div.obj-overlay.anim buttons]]
           (or credits
               [:div.credits
                [:span (str (.toUpperCase title) " by " (.toUpperCase author))]
                [:span (utils/format-date-time (js/Date. created))]]))))
        (dom/query ".obj-preview")
        (dom/set-style! (clj->js {:background-image (str "url(" img-url ")")})))
    (when (or approve edit download)
      (dom/add-listeners
       (cond->
        [(item-listener
          item ".obj-preview" "mouseover"
          (fn [] (async/publish bus :focus-gallery-item [:on item obj])))
         (item-listener
          item ".obj-overlay" "mouseleave"
          (fn [] (async/publish bus :focus-gallery-item [:off item obj])))
         (item-listener
          item ".obj-preview" "touchstart"
          (fn [e] (.stopPropagation e) (async/publish bus :focus-gallery-item [:on item obj])))
         (item-listener
          item ".obj-overlay" "touchstart"
          (fn [e] (.stopPropagation e) (async/publish bus :focus-gallery-item [:off item obj])))]
        edit     (conj (item-listener
                        item ".obj-edit" "click"
                        (fn [e] (.stopPropagation e) (route/set-route! "objects" id))))
        download (conj (item-listener
                        item ".obj-download" "click"
                        (fn [e] (.stopPropagation e) (route/set-location! stl-url))))
        info     (conj (item-listener
                        item ".obj-info" "click"
                        (fn [e] (.stopPropagation e) (route/set-route! "gallery" id))))
        approve  (conj (item-listener
                        item ".obj-approve" "click"
                        (fn [e] (.stopPropagation e) (async/publish bus :approve-gallery-item id)))))))))

(defn handle-item-overlay
  [ch state parent]
  (go
    (while true
      (let [[_ [cmd item obj]] (<! ch)
            focused            (:focused @state)
            on?                (or (= :on cmd) (not= focused item))]
        (if focused
          (-> (dom/query focused ".obj-overlay")
              (dom/set-style! #js {:visibility "hidden"})
              (dom/remove-class! "fade-in")))
        (if on?
          (-> (dom/query item ".obj-overlay")
              (dom/set-style! #js {:visibility "visible"})
              (dom/add-class! "fade-in")))
        (swap! state assoc :focused (if on? item))))))
