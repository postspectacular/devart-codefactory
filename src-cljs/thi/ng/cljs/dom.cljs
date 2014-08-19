(ns thi.ng.cljs.dom
  (:require
   [goog.style :as style]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]
   [cljs.core.async :refer [chan put! close!]]))

(def svg-ns "http://www.w3.org/2000/svg")

(defn wheel-event-type
  [] (if (.isDef js/goog (.-onwheel js/window)) "wheel" "mousewheel"))

(defn by-id
  [id] (.getElementById js/document id))

(defn query
  [e q] (.querySelector (or e js/document) q))

(defn query-all
  [e q] (.querySelectorAll (or e js/document) q))

(defn set-html!
  [el s] (set! (.-innerHTML el) s) el)

(defn set-text!
  [el s] (dom/setTextContent el s) el)

(defn clear!
  [el] (set-html! el ""))

(defn set-class! [el name]
  (classes/set el name) el)

(defn add-class!
  [el name]
  (classes/add el name) el)

(defn remove-class!
  [el name] (classes/remove el name) el)

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
          (recur (next attribs))))))
  el)

(defn remove-attribs!
  [el attribs]
  (if el
    (loop [attribs attribs]
      (when attribs
        (.removeAttribute el (name (first attribs)))
        (recur (next attribs)))))
  el)

(defn set-style!
  [el opts]
  (style/setStyle el opts)
  el)

(defn get-style
  [el prop]
  (style/getStyle el (name prop)))

(defn show!
  [el] (style/setStyle el "display" "block"))

(defn hide!
  [el] (style/setStyle el "display" "none"))

(defn offset
  [el] [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

(defn size
  [el] [(.-clientWidth el) (.-clientHeight el)])

(defn request-fullscreen
  []
  (let [doc (.-documentElement js/document)]
    (cond
     (.-requestFullscreen doc) (.requestFullscreen doc)
     (.-mozRequestFullScreen doc) (.mozRequestFullScreen doc)
     (.-webkitRequestFullscreen doc) (.webkitRequestFullscreen doc)
     (.-msRequestFullscreen doc) (.msRequestFullscreen doc)
     :default nil)))

(defn match-media
  [q] (.-matches (.matchMedia js/window q)))

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
  ([ns type parent] (create-ns! ns type parent nil))
  ([ns type parent attribs]
     (let [el (.createElementNS js/document ns type)]
       (when parent
         (.appendChild parent el))
       (when attribs
         (set-attribs! el attribs))
       el)))

(defn force-redraw!
  [el]
  (set-style! el #js {:display "none"})
  (set-style! el #js {:display "block"}))

(defn- update-listeners*
  [update! specs]
  (loop [specs specs]
    (if specs
      (let [[el eid f cap?] (first specs)
            el (if (string? el)
                 (if (= "$window" el)
                   js/window (query nil el))
                 el)]
        (when el (update! el (name eid) f cap?))
        (recur (next specs)))))
  specs)

(defn add-listeners
  [specs]
  (update-listeners*
   (fn [el eid f cap?]
     (.addEventListener el eid f cap?))
   specs))

(defn remove-listeners
  [specs]
  (update-listeners*
   (fn [el eid f _]
     (.removeEventListener el eid f))
   specs))
