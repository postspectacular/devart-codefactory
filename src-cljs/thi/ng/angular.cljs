(ns thi.ng.angular)

(def event-canvas-ready "canvas-ready")
(def event-resize-canvas "resize-canvas")

(defn assoc-state
  [state ks v]
  (swap! state assoc-in ks v))

(defn update-state
  [state ks fn]
  (swap! state update-in ks fn))

(defn merge-state
  [state props]
  (swap! state merge props))

(defn parse-int
  [x nf]
  (let [x' (js/parseInt x)]
    (if (js/isNaN x') nf x)))

(defn enabled-attrib?
  [jsobj k]
  (let [v (aget jsobj k)]
    (not (or (nil? v) (false? v) (= "false" v)))))

(defn make-canvas
  [w h]
  (let [canvas (.createElement js/document "canvas")]
    (doto canvas
      (.setAttribute "width" w)
      (.setAttribute "height" h))))

(.. js/angular
    (module "thi.ng.ngSupport" #js [])

    (directive
     "resizableCanvas"
     #js ["$window"
          (fn [$window]
            #js
            {:restrict "E"
             :transclude true
             :scope #js {:width "="
                         :height "="}
             :link (fn [scope element attribs]
                     (let [get-width (fn []
                                       (let [width (.-width scope)]
                                         (if (<= width 1)
                                           (int (* width (.-innerWidth $window)))
                                           (int width))))
                           get-height (fn []
                                        (let [height (.-height scope)]
                                          (if (<= height 1)
                                            (int (* height (.-innerHeight $window)))
                                            (int height))))]

                       (set! (.-state scope)
                             (atom
                              {:init (fn []
                                       (let [canvas (make-canvas (get-width) (get-height))]
                                         (assoc-state (.-state scope) [:canvas] canvas)
                                         (.appendChild (aget element 0) canvas)
                                         (prn "calling GL init hook...")
                                         (.$emit scope event-canvas-ready canvas)
                                         ((:resize @(.-state scope)))))
                               :resize (fn []
                                         (let [canvas (:canvas @(.-state scope))]
                                           (set! (.-width canvas) (get-width))
                                           (set! (.-height canvas) (get-height))
                                           (.$emit scope event-resize-canvas canvas)))}))

                       (.addEventListener $window "resize" (:resize @(.-state scope)) false)
                       (.$watch scope "width + height" (fn [] (:resize @(.-state scope))))

                       ((:init @(.-state scope)))))})]))
