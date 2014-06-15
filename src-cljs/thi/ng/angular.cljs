(ns thi.ng.angular
  (:require-macros [hiccups.core :as h])
  (:require
   [thi.ng.geom.webgl.core :as gl]
   [hiccups.runtime]))

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
     "webglCanvas"
     #js ["$window"
          (fn [$window]
            #js
            {:restrict "E"
             :transclude true
             :scope #js {:width "="
                         :height "="
                         :fill "="
                         :init-hook "&onInit"}
             :link (fn [scope element attribs]
                     (let [width (parse-int (aget attribs "width") 200)
                           height (parse-int (aget attribs "height") 200)
                           fill? (enabled-attrib? attribs "fill")
                           parent (.-parentNode (aget element 0))]

                       (set! (.-init scope)
                             (fn []
                               (let [canvas (make-canvas width height)
                                     ctx (gl/gl-context canvas)]
                                 (set! (.-canvas scope) canvas)
                                 (set! (.-ctx scope) ctx)
                                 (.appendChild (aget element 0) canvas))))

                       (set! (.-resizeCanvas scope)
                             (fn []
                               (let [canvas (.-canvas scope)]
                                 (set! (.-width canvas)
                                       (if fill?
                                         (.-clientWidth parent)
                                         (.-width canvas)))
                                 (set! (.-height canvas) (.-height canvas)))))

                       (.addEventListener $window "resize" (.-resizeCanvas scope) false)
                       (.$watch scope "fill + width + height" (fn [] (.resizeCanvas scope)))
                       
                       (.init scope)))})]))
