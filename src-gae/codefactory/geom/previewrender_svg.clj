(ns codefactory.geom.previewrender-svg
  (:require
   [thi.ng.geom.core :as g]
   [codefactory.geom.viewfinder :as vf]
   [hiccup.core :as h]))

(defn svg
  [attrs & body]
  [:svg
   (merge
    {:xmlns "http://www.w3.org/2000/svg"
     :version "1.1"}
    attrs)
   body])

(defn svg->bytes
  [svg] (.getBytes (h/html {:mode :xml} svg) "UTF-8"))

(defn draw-triangle
  [[[ax ay] [bx by] [cx cy]]]
  [:path {:d (format "M%1.2f,%1.2f L%1.2f,%1.2f L%1.2f,%1.2f Z"
                     ax ay bx by cx cy)}])

(defn draw-mesh
  [mesh mvp vtx]
  (map
   (fn [f]
     (draw-triangle (mapv (fn [p] (vf/project-point p mvp vtx)) f)))
   (g/faces mesh)))

(defn render-mesh
  [mesh model view proj width height attribs]
  (let [mvp (g/* proj (g/* view model))
        vtx (vf/viewport-transform width height)
        svg (svg
             (merge {:width width :height height} attribs)
             (draw-mesh mesh mvp vtx))]
    (prn :svg svg)
    svg))
