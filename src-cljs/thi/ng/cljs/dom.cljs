(ns thi.ng.cljs.dom
  (:require
   [goog.style :as style]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]))

(defn by-id [id]
  (.getElementById js/document id))

(defn query
  [e q]
  (.querySelector (or e js/document) q))

(defn query-all
  [e q]
  (.querySelectorAll (or e js/document) q))

(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(defn set-text! [el s]
  (dom/setTextContent el s))

(defn set-class! [el name]
  (classes/set el name))

(defn add-class! [el name]
  (classes/add el name))

(defn remove-class! [el name]
  (classes/remove el name))

(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

(defn size
  [el]
  (let [s (style/getSize el)]
    [(.-width s) (.-height s)]))

(defn request-fullscreen
  []
  (let [doc (.-documentElement js/document)]
    (cond
     (.-requestFullscreen doc) (.requestFullscreen doc)
     (.-mozRequestFullscreen doc) (.mozRequestFullscreen doc)
     (.-webkitRequestFullscreen doc) (.webkitRequestFullscreen doc)
     (.-msRequestFullscreen doc) (.msRequestFullscreen doc)
     :default nil)))

(defn match-media
  [q]
  (.-matches (.matchMedia js/window q)))

(defn parent
  [el] (.-parentElement el))