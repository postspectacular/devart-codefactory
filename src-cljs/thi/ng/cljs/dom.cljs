(ns thi.ng.cljs.dom
  (:require
   [thi.ng.cljs.utils :as utils]
   [clojure.string :as str]
   [goog.style :as style]
   [goog.dom :as dom]
   [goog.dom.classes :as classes]))

(def svg-ns "http://www.w3.org/2000/svg")

(def re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def svg-tags #{"svg" "g" "path" "rect" "circle" "line" "polyline" "polygon" "text"})

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
  (if (string? name)
    (classes/add el name)
    (dorun (map #(classes/add el %) name)))
  el)

(defn remove-class!
  [el name]
  (classes/remove el name) el)

(defn set-style!
  [el opts]
  (style/setStyle el (clj->js opts))
  el)

(defn get-style
  [el prop]
  (style/getStyle el (name prop)))

(defn get-attrib
  [el attr] (.getAttribute el attr))

(defn get-attribs
  [el attrs]
  (map #(.getAttribute el %) attrs))

(defn set-attribs!
  [el attribs]
  (if el
    (loop [attribs attribs]
      (if attribs
        (let [[k v] (first attribs)]
          (if v
            (if (= :style k)
              (set-style! el v)
              (.setAttribute el (name k) v)))
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
     (let [el (if (svg-tags type)
                (.createElementNS js/document svg-ns type)
                (.createElement js/document type))]
       (when parent
         (.appendChild parent el))
       (when attribs
         (set-attribs! el attribs))
       el)))

(defn remove!
  [el] (.removeChild (.-parentElement el) el))

(defn append!
  [parent el] (.appendChild parent el))

(defn insert!
  [el parent]
  (.insertBefore parent el (.-firstChild parent))
  el)

(defn create-text!
  [txt parent]
  (let [el (.createTextNode js/document txt)]
    (.appendChild parent el)
    el))

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
  (set-style! el {:display "none"})
  (set-style! el {:display "block"}))

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

(defn normalize-element
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (str tag " is not a valid tag name")))
  (let [[_ tag id class] (re-matches re-tag (utils/as-str tag))
        tag-attrs        {:id id
                          :class (if class (str/replace class "." " "))}
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn create-dom!
  [x parent]
  (cond
   (vector? x) (let [[tag attrs content] (normalize-element x)
                     el (create! tag parent attrs)]
                 (when content (create-dom! content el))
                 el)
   (seq? x)    (doall (map #(create-dom! % parent) x))
   (nil? x)    parent
   :else       (create-text! (utils/escape-html x) parent)))
