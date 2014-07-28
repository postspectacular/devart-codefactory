(ns codefactory.webgl
  (:require-macros
   [thi.ng.macromath.core :as mm])
  (:require
   [codefactory.color :as col]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.webgl.shaders :as sh]
   [thi.ng.geom.webgl.utils :refer [loop-kv]]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.matrix :as mat :refer [M44]]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as poly]
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.common.math.core :as m :refer [HALF_PI]]
   ))

(def lambert-shader-spec
  {:vs "
void main() {
  vec3 tNormal = (normalMat * vec4(normal, 0.0)).xyz;

  float lambert = min(max(dot(tNormal, lightDir) + 0.2, 0.0) * 0.8, 1.0);
  vCol = vec4(ambientCol + lightCol * lambert, alpha);
  gl_Position = proj * view * model * vec4(position, 1.0);
}"
   :fs "void main() { gl_FragColor = vCol; }"
   :uniforms {:model      :mat4
              :view       :mat4
              :normalMat  :mat4
              :proj       :mat4
              :ambientCol :vec3
              :lightCol   :vec3
              :lightDir   :vec3
              :alpha      :float}
   :attribs  {:position   :vec3
              :normal     :vec3}
   :varying  {:vCol       :vec4}})

(def xray-shader-spec
  {:vs "
void main() {
  vec4 worldPos = view * model * vec4(position, 1.0);
  vNormal = (normalMat * vec4(normal, 0.0)).xyz;
  vIncident = worldPos.xyz;
  gl_Position = proj * worldPos;
}"
   :fs "
void main() {
  float opac = abs(dot(normalize(-vNormal), normalize(-vIncident)));
  opac = 1.0-pow(opac, alpha);
  gl_FragColor = vec4(lightCol * opac, opac);
}"
   :uniforms {:model      :mat4
              :view       :mat4
              :normalMat  :mat4
              :proj       :mat4
              :lightCol   :vec3
              :alpha      :float}
   :attribs  {:position   :vec3
              :normal     :vec3}
   :varying  {:vIncident  :vec3
              :vNormal    :vec3}})

(def shader-presets
  {:lambert-default
   {:spec     lambert-shader-spec
    :state    {:depth-test true}
    :uniforms {:ambientCol [0.3 0.3 0.3]
               :lightCol [0.75 0.75 0.75]
               :lightDir (g/normalize (vec3 0 1 1))
               :alpha 1.0}}

   :lambert-xray
   {:spec     lambert-shader-spec
    :state    {:depth-test false
               :blend true
               :blend-func [gl/src-alpha gl/one]}
    :uniforms {:ambientCol [0.1 0.1 0.1]
               :lightCol [0.5 0.5 0.5]
               :lightDir (g/normalize (vec3 0 1 1))
               :alpha 0.25}}

   :xray-strong
   {:spec     xray-shader-spec
    :state    {:depth-test false
               :blend true
               :blend-func [gl/src-alpha gl/one]}
    :uniforms {:lightCol [0.5 0.5 0.5] :alpha 0.5}}

   :xray-soft
   {:spec     xray-shader-spec
    :state    {:depth-test false
               :blend true
               :blend-func [gl/src-alpha gl/one]}
    :uniforms {:lightCol [0.75 0.75 0.75] :alpha 0.25}}
   })

(defn init-shader
  [gl preset-id]
  (let [preset (shader-presets preset-id)
        shader (sh/make-shader-from-spec gl (:spec preset))]
    {:shader shader
     :preset-id preset-id
     :preset preset}))

(defn init-shaders
  [{:keys [gl] :as state} preset-ids]
  (assoc state
    :shaders (mapv #(init-shader gl %) preset-ids)))

(defn init-webgl
  [canvas config]
  (try
    (let [aa? (not (dom/match-media (str "(max-width: " (:min-aa-res config) "px)")))
          gl (gl/gl-context canvas {:antialias aa?})]
      (-> {:canvas canvas :gl gl}
          (init-shaders (:shader-preset-ids config))))
    (catch js/Error e false)))

(defn mesh-buffer
  [gl mesh]
  (-> mesh
      (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
      (buf/make-attribute-buffers-in-spec gl gl/static-draw)))

(defn delete-meshes
  [gl meshes]
  (loop [meshes meshes]
    (when meshes
      (let [{{:keys [position normal]} :attribs} (first meshes)]
        (debug :delete-buffer position normal)
        (.deleteBuffer gl (:buffer position))
        (.deleteBuffer gl (:buffer normal))
        (recur (next meshes))))))

(defn prepare-render-state
  [gl state]
  (if (:depth-test state)
    (gl/enable gl gl/depth-test)
    (gl/disable gl gl/depth-test))
  (if (:blend state)
    (doto gl
      (gl/enable gl/blend)
      (.blendFunc gl/src-alpha gl/one))
    (gl/disable gl gl/blend)))

(defn draw-meshes
  [gl meshes shader uniforms]
  (.useProgram gl (:program shader))
  (loop-kv #(sh/set-uniform shader % %2) uniforms)
  (loop [meshes meshes]
    (if meshes
      (let [{:keys [attribs num-vertices]} (first meshes)]
        (loop-kv #(sh/set-attribute gl shader % %2) attribs)
        (.drawArrays gl gl/triangles 0 num-vertices)
        (recur (next meshes))))))

(defn render-meshes
  [gl shader meshes shared uniforms]
  (prepare-render-state gl (:state (:preset shader)))
  (draw-meshes
   gl meshes (:shader shader)
   (merge (:uniforms (:preset shader)) shared uniforms)))

(defn render-with-selection
  [gl shaders shared-uniforms meshes sel-meshes sel-color time sel-time]
  (let [[xray solid] shaders
        color (col/pulsate 0.5 sel-color time 6)
        alpha (-> xray :preset :uniforms :alpha)]
    (render-meshes
     gl solid sel-meshes shared-uniforms {:lightCol color})
    (render-meshes
     gl xray meshes shared-uniforms {:alpha alpha})))

(defn render-axes
  [gl shader uniforms axes]
  (render-meshes
   gl shader [(axes 0)] uniforms {:lightCol [1 0 0]})
  (render-meshes
   gl shader [(axes 1)] uniforms {:lightCol [0 1 0]})
  (render-meshes
   gl shader [(axes 2)] uniforms {:lightCol [0 0 1]}))

(defn axis-meshes
  [gl radius len]
  (let [z (-> (c/circle radius)
              (g/extrude {:depth len :bottom? false :res 8})
              (g/as-mesh {:mesh (bm/basic-mesh)}))
        x (g/transform z (g/rotate-y M44 HALF_PI))
        y (g/transform z (g/rotate-x M44 (- HALF_PI)))
        lx (poly/polygon2 [[0 0] [0.976 0] [3.03 4.16] [5.1 0] [6.06 0] [3.51 5.14]
                           [5.91 10] [4.94 10] [3.03 6.13] [1.12 10] [0.15 10] [2.55 5.14]])
        ly (poly/polygon2 [[0 10] [2.59 4.77] [2.59 0] [3.49 0] [3.49 4.77]
                           [6.05 10] [5.08 10] [3.03 5.84] [0.97 10]])
        lz (poly/polygon2 [[0.14 10] [0.14 9.14] [4.46 9.14] [0 0.43] [0 0]
                           [5.63 0] [5.63 0.86] [1.17 0.86] [5.63 9.57] [5.63 10]])
        loff (+ len 0.1)]
    (reduce
     (fn [specs [m l o]]
       (conj specs
             (-> (g/into m (-> l
                               (g/scale 0.01)
                               (g/center)
                               (g/extrude {:depth 0.01 :mesh (bm/basic-mesh)})
                               (g/translate o)))
                 (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                 (buf/make-attribute-buffers-in-spec gl gl/static-draw))))
     [] [[x lx (vec3 loff 0 0)]
         [y ly (vec3 0 loff 0)]
         [z lz (vec3 0 0 loff)]])))
