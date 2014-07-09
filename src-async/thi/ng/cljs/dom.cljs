(ns thi.ng.cljs.dom
  (:require
   [goog.style :as style]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]
   [cljs.core.async :refer [chan put! close!]]))

(defn wheel-event-type
  [] (if (.isDef js/goog (.-onwheel js/window)) "wheel" "mousewheel"))

(defn by-id
  [id] (.getElementById js/document id))

(defn query
  [e q] (.querySelector (or e js/document) q))

(defn query-all
  [e q] (.querySelectorAll (or e js/document) q))

(defn set-html!
  [el s] (set! (.-innerHTML el) s))

(defn set-text!
  [el s] (dom/setTextContent el s))

(defn set-class! [el name]
  (classes/set el name))

(defn add-class!
  [el name] (classes/add el name))

(defn remove-class!
  [el name] (classes/remove el name))

(defn get-attrib
  [el attr] (.getAttribute el attr))

(defn get-attribs
  [el attrs]
  (map (fn [a] (get-attrib el a)) attrs))

(defn set-attribs!
  [el attribs]
  (if el
    (loop [attribs attribs]
      (if attribs
        (let [[k v] (first attribs)]
          (.setAttribute el (name k) v)
          (recur (next attribs)))))))

(defn set-style!
  [el opts]
  (style/setStyle el opts))

(defn show
  [el] (style/setStyle el "display" "block"))

(defn hide
  [el] (style/setStyle el "display" "none"))

(defn offset
  [el] [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

(defn size
  [el] [(.-clientWidth el) (.-clientHeight el)])

(defn remove-children
  [el] (while (.-lastChild el) (.removeChild el (.-lastChild el))))

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

(defn create!
  ([type] (create! type nil nil))
  ([type parent] (create! type parent nil))
  ([type parent attribs]
     (let [el (.createElement js/document type)]
       (when parent
         (.appendChild parent el))
       (when attribs
         (set-attribs! el attribs))
       el)))

(defn remove!
  [el] (.removeChild (.-parentElement el) el))

(defn insert!
  [el parent]
  (.insertBefore parent el (.-firstChild parent))
  el)

(defn create-ns!
  [ns type parent]
  (let [el (.createElementNS js/document ns type)]
    (when parent
      (.appendChild parent el))
    el))

(defn force-redraw!
  [el]
  (set-style! el #js {:display "none"})
  (set-style! el #js {:display "block"}))

(defn event-channel
  [el id]
  (let [el (if (string? el) (query nil el) el)
        ch (chan)
        f (fn [e] (.preventDefault e) (put! ch e))]
    (.addEventListener el id f)
    [ch f id el]))

(defn destroy-event-channel
  [[ch f ev el]]
  (.removeEventListener el ev f)
  (close! ch))
