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
   [thi.ng.geom.rect :as r]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.common.math.core :as m]
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
        color sel-color
        ;; color (col/pulsate 0.5 sel-color time 0.5)
        alpha (get-in xray [:preset :uniforms :alpha])
        ;; alpha (m/mix 1.0 alpha (min (mm/subm time sel-time 0.2) 1.0))
        ]
    (render-meshes
     gl solid sel-meshes shared-uniforms {:lightCol color})
    (render-meshes
     gl xray meshes shared-uniforms {:alpha alpha})))
