(ns codefactory.common
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.dom :as dom]
   [clojure.string :as str]))

(defn loader-html
  [msg parent]
  (dom/create-dom!
   [:div.loading
    [:p msg]
    [:img {:src "/img/loading.gif" :alt "loading"}]]
   parent))

(defn show-nav
  [] (dom/remove-class! (dom/query nil "nav") "hidden"))

(defn icon-button
  [parent id [w h] paths label handler & classes]
  (let [attrs  {:class (apply str "tool " classes)}
        el     (dom/create-dom!
                [:div (if id (assoc attrs :id (name id)) attrs)
                 [:svg {:width w
                        :height h
                        :viewBox "-0.05 -0.05 1.1 1.1"
                        :preserveAspectRatio "none"}
                  (map (fn [p] [:path p]) paths)]
                 (if label [:div label])]
                parent)]
    (first
     (if handler
       (dom/add-listeners [[el "click" handler]])
       [[el]]))))

(defn next-parent-id
  [el]
  (loop [el el]
    (if-let [id (first (dom/get-attribs el ["id"]))]
      id
      (if-let [el (dom/parent el)]
        (recur el)))))

(defn item-asset-url
  [item type]
  (if (route/local?)
    (config/api-route :object-asset (:id item) type)
    (str/replace-first
     (item (keyword (str (name type) "-uri")))
     "https://" "http://")))
