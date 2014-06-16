(ns thi.ng.arcball
  (:require-macros [thi.ng.macromath.core :as mm])
  (:require
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec2 vec3 V3Y]]
   [thi.ng.geom.core.matrix :as mat]
   [thi.ng.geom.core.quaternion :as q]
   [thi.ng.common.math.core :as m]))

(defn pos->sphere
  [p r x y]
  (let [v (vec3 (mm/subdiv x (p 0) r) (- (mm/subdiv y (p 1) r)) 0)
        m (g/mag-squared v)]
    (if (> m 1.0) (g/normalize v) (assoc v :z (Math/sqrt (- 1 m))))))

(defprotocol PArcBall
  (listen! [_ callback])
  (unlisten! [_])
  (down [_ x y])
  (drag [_ x y])
  (up [_])
  (resize [_ w h])
  (zoom [_ delta])
  (update-view [_])
  (get-view [_]))

(deftype ArcBall
    [min-dist max-dist
     ^:mutable radius
     ^:mutable center
     ^:mutable dist
     ^:mutable curr-rot
     ^:mutable click-rot
     ^:mutable click-pos
     ^:mutable view
     ^:mutable callback
     ^:mutable listeners]
  PArcBall
  (listen!
    [_ cb]
    (let [ldown (fn [e] (down _ (.-x e) (- (.-innerHeight js/window) (.-y e))))
          lup   (fn [e] (up _))
          ldrag (fn [e] (if click-pos (drag _ (.-x e) (- (.-innerHeight js/window) (.-y e)))))
          lzoom (fn [e] (zoom _ (.-wheelDeltaY e)) (.preventDefault e))
          lresize (fn [e] (resize _ (.-innerWidth js/window) (.-innerHeight js/window)))]
      (set! callback cb)
      (set! listeners {:down ldown :up lup :drag :ldrag :zoom lzoom :resize lresize})
      (.addEventListener js/window "mousedown" ldown)
      (.addEventListener js/window "mouseup" lup)
      (.addEventListener js/window "mousemove" ldrag)
      (.addEventListener js/window "mousewheel" lzoom)
      (.addEventListener js/window "resize" lresize)
      _))
  (unlisten!
    [_]
    (.removeEventListener js/window "mousedown" (:down listeners))
    (.removeEventListener js/window "mouseup" (:up listeners))
    (.removeEventListener js/window "mousemove" (:drag listeners))
    (.removeEventListener js/window "mousewheel" (:zoom listeners))
    (.removeEventListener js/window "resize" (:resize listeners))
    _)
  (down
    [_ x y]
    (set! click-pos (pos->sphere center radius x y))
    (set! click-rot (q/quat curr-rot))
    (update-view _))
  (drag
    [_ x y]
    (let [drag-pos (pos->sphere center radius x y)
          axis (g/cross click-pos drag-pos)
          theta (g/dot click-pos drag-pos)
          drag-rot (q/quat axis theta)]
      (set! curr-rot (g/* drag-rot click-rot))
      (update-view _)))
  (up [_] (set! click-pos nil))
  (resize
    [_ w h]
    (let [ww (/ (.-innerWidth js/window) 2)
          wh (/ (.-innerHeight js/window) 2)]
      (set! radius (* (min ww wh) 2))
      (set! center (vec2 ww wh))
      _))
  (zoom
    [_ delta]
    (set! dist (m/clamp (mm/madd delta (mm/subm max-dist min-dist 2e-4) dist) min-dist max-dist))
    (update-view _))
  (update-view
    [_]
    (let [q (q/quat (:xyz curr-rot) (- (:w curr-rot)))
          offset (g/transform (vec3 0 0 dist) q)
          eye (g/- offset)
          target (vec3)
          up (g/transform V3Y q)]
      (set! view (mat/look-at eye target up))
      (when callback (callback view))
      _))
  (get-view [_] view))

(defn make-arcball
  [& {:keys [dist min-dist max-dist radius center] :or {dist 2}}]
  (let [min-dist (or min-dist (/ dist 2))
        max-dist (or max-dist (* dist 2))
        curr-rot (q/quat-from-axis-angle V3Y m/PI)
        a (ArcBall. min-dist max-dist radius center dist curr-rot (vec3) nil nil nil)]
    (-> a (resize) (update-view))))
