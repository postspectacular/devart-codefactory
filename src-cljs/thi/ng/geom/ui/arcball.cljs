(ns thi.ng.geom.ui.arcball
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
  (listen! [_ el callback])
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
     ^:mutable listeners]
  PArcBall
  (listen!
    [_ el cb]
    (let [ldown (fn [e]
                  (let [y (- (.-y e) (.-offsetTop el))
                        h (.-clientHeight el)]
                    (if (m/in-range? 0 h y)
                      (down _ (.-x e) (- h y)))))
          lup   (fn [e] (up _))
          ldrag (fn [e]
                  (if click-pos
                    (drag _ (.-x e) (- (.-clientHeight el) (- (.-y e) (.-offsetTop el))))))
          lzoom (fn [e] (zoom _ (.-wheelDeltaY e)) (.preventDefault e))
          lresize (fn [] (resize _ (.-clientWidth el) (.-clientHeight el)))]
      (set! listeners
            {:down ldown :up lup :drag ldrag
             :zoom lzoom :resize lresize
             :callback cb :element el})
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
    (set! listeners nil)
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
    (let [ww (/ w 2)
          wh (/ h 2)]
      (set! radius (* (min ww wh) 2))
      (set! center (vec2 ww wh))
      (prn :center center)
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
      (when-let [callback (:callback listeners)]
        (callback view))
      view))
  (get-view [_] (or view (update-view _))))

(defn make-arcball
  [& {:keys [init dist min-dist max-dist radius center] :or {dist 2}}]
  (let [min-dist (or min-dist (/ dist 2))
        max-dist (or max-dist (* dist 2))
        curr-rot (if init (q/quat init) (q/quat-from-axis-angle V3Y m/PI))]
    (ArcBall. min-dist max-dist radius center dist curr-rot (vec3) nil nil nil)))