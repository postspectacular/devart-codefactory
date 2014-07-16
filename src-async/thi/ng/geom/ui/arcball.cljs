(ns thi.ng.geom.ui.arcball
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec2 vec3 V3Y]]
   [thi.ng.geom.core.matrix :as mat]
   [thi.ng.geom.core.quaternion :as q]
   [thi.ng.common.math.core :as m]))

(defprotocol PArcBall
  (down [_ x y])
  (drag [_ x y])
  (up [_])
  (resize [_ w h])
  (zoom-delta [_ delta])
  (zoom-abs [_ x])
  (update-view [_])
  (get-view [_])
  (get-rotation [_])
  (set-rotation [_ q]))

(defn pos->sphere
  [p r x y]
  (let [v (vec3 (mm/subdiv x (p 0) r) (- (mm/subdiv y (p 1) r)) 0)
        m (g/mag-squared v)]
    (if (> m 1.0)
      (g/normalize v)
      (assoc v :z (Math/sqrt (- 1 m))))))

(deftype ArcBall
    [min-dist max-dist
     ^:mutable radius
     ^:mutable center
     ^:mutable dist
     ^:mutable curr-rot
     ^:mutable click-rot
     ^:mutable click-pos
     ^:mutable view]
  PArcBall
  (down
    [_ x y]
    (set! click-pos (pos->sphere center radius x y))
    (set! click-rot (q/quat curr-rot))
    (update-view _))
  (drag
    [_ x y]
    (when click-pos
      (let [drag-pos (pos->sphere center radius x y)
            axis (g/cross click-pos drag-pos)
            theta (g/dot click-pos drag-pos)
            drag-rot (q/quat axis theta)]
        (set! curr-rot (g/* drag-rot click-rot))
        (update-view _))))
  (up
    [_] (set! click-pos nil))
  (resize
    [_ w h]
    (let [ww (/ w 2)
          wh (/ h 2)]
      (set! radius (* (min ww wh) 2))
      (set! center (vec2 ww wh))
      _))
  (zoom-delta
    [_ delta]
    (set! dist (m/clamp (mm/madd delta (mm/subm max-dist min-dist 1e-3) dist) min-dist max-dist))
    (update-view _))
  (zoom-abs
    [_ x]
    (set! dist (m/clamp x min-dist max-dist))
    (update-view _))
  (update-view
    [_]
    (let [q (q/quat (:xyz curr-rot) (- (:w curr-rot)))
          offset (g/transform (vec3 0 0 dist) q)
          eye (g/- offset)
          target (vec3)
          up (g/transform V3Y q)]
      (set! view (mat/look-at eye target up))
      (prn :arcball (pr-str curr-rot) dist)
      view))
  (get-view
    [_] (or view (update-view _)))
  (get-rotation
    [_] curr-rot)
  (set-rotation
    [_ q]
    (set! curr-rot q)
    (update-view _)))

(defn make-arcball
  [& {:keys [init dist min-dist max-dist radius center] :or {dist 2.75}}]
  (let [min-dist (or min-dist (/ dist 2))
        max-dist (or max-dist (* dist 2))
        curr-rot (if init (q/quat init) (q/quat-from-axis-angle V3Y m/PI))]
    (ArcBall. min-dist max-dist radius center dist curr-rot (vec3) nil nil)))
