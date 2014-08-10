(ns codefactory.geom.previewrender-bitmap
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat]
   [codefactory.geom.viewfinder :as vf])
  (:import
   [java.io ByteArrayOutputStream]
   [java.awt Color Graphics2D RenderingHints BasicStroke]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]))

(defn ^BufferedImage make-image
  "Creates a new ARGB BufferedImage of the given size."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))

(defn image->png-bytes
  [^BufferedImage img]
  (with-open [out (ByteArrayOutputStream. 0x100000)]
    (ImageIO/write img "PNG" out)
    (.toByteArray out)))

(defn clear-image
  [^Graphics2D gfx width height]
  (doto gfx
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setPaint (Color. 255 255 255))
    (.fillRect 0 0 width height)))

(defn draw-line
  [gfx [ax ay] [bx by]]
  (.drawLine gfx (int ax) (int ay) (int bx) (int by)))

(defn draw-triangle
  [gfx [a b c]]
  (draw-line gfx a b)
  (draw-line gfx b c)
  (draw-line gfx c a))

(defn draw-mesh
  [gfx mesh mvp vtx]
  (doseq [f (g/faces mesh)]
    (draw-triangle gfx (mapv #(mat/project-point % mvp vtx) f))))

(defn render-mesh
  [mesh model view proj width height]
  (let [img (make-image width height)
        gfx (.getGraphics img)
        mvp (g/* proj (g/* view model))
        vtx (mat/viewport-transform width height)]
    (doto gfx
      (clear-image width height)
      (.setStroke (BasicStroke. 1))
      (.setPaint Color/RED)
      (draw-mesh mesh mvp vtx))))

