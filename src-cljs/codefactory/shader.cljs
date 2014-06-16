(ns codefactory.shader
  (:require
   [codefactory.config :as config]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :refer [vec3]]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.shaders :as sh]
   [thi.ng.geom.webgl.utils :refer [loop-kv]]))

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

(def presets
  {:lambert-default
   {:spec     lambert-shader-spec
    :state    {:depth-test true}
    :uniforms {:ambientCol [0.3 0.3 0.3]
               :lightCol [0.8 0.8 0.8]
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

;; normal: gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

(defn prepare-state
  [ctx state]
  (if (:depth-test state)
    (gl/enable ctx gl/depth-test)
    (gl/disable ctx gl/depth-test))
  (if (:blend state)
    (do
      (gl/enable ctx gl/blend)
      (.blendFunc ctx gl/src-alpha gl/one))
    (gl/disable ctx gl/blend)))

(defn draw-meshes
  [gl meshes shader uniforms]
  (.useProgram gl (:program shader))
  (loop-kv #(sh/set-uniform shader % %2) uniforms)
  (loop-kv
   (fn [_ {:keys [attribs num-vertices]}]
     (loop-kv #(sh/set-attribute gl shader % %2) attribs)
     (.drawArrays gl gl/triangles 0 num-vertices)
     ;;(loop-kv (fn [id _] (sh/disable-attribute gl shader id)) attribs)
     )
   meshes))
