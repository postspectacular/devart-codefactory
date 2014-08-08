(ns codefactory.geom.previewrender-svg
  (:require
   [codefactory.geom.projection :as proj]
   [thi.ng.geom.core :as g]
   [hiccup.core :as h]))

(def ^:const xml-preamble "<?xml version=\"1.0\"?>\n")

(defn svg
  [attrs & body]
  [:svg
   (merge
    {:xmlns "http://www.w3.org/2000/svg"
     :version "1.1"}
    attrs)
   body])

(defn svg->bytes
  [svg]
  (.getBytes
   (str xml-preamble (h/html {:mode :xml} svg))
   "UTF-8"))

(defn svg-triangle
  [[[ax ay] [bx by] [cx cy]]]
  [:path {:d (format "M%1.2f,%1.2f L%1.2f,%1.2f L%1.2f,%1.2f Z"
                     ax ay bx by cx cy)}])

(defn svg-mesh
  [mesh mvp vtx]
  (map
   (fn [f] (svg-triangle (mapv (fn [p] (proj/project-point p mvp vtx)) f)))
   (g/faces mesh)))

(defn render-mesh
  [mesh model view proj width height attribs]
  (let [mvp (g/* proj (g/* view model))
        vtx (proj/viewport-transform width height)
        svg (svg
             (merge {:width width :height height} attribs)
             (svg-mesh mesh mvp vtx))]
    ;; (prn :svg svg)
    svg))
