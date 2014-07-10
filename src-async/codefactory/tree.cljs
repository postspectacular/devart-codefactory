(ns codefactory.tree
  (:require
   [codefactory.config :as config]
   [thi.ng.cljs.async :as async]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.geom.webgl.core :as gl]
   [thi.ng.geom.webgl.buffers :as buf]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.basicmesh :as bm]
   [thi.ng.morphogen.core :as mg]
   [thi.ng.common.math.core :as m]))

(def direction-ids [:x :y :z])
(def direction-idx {:x 0 :y 1 :z 2})
(def face-ids      [:e :w :n :s :f :b])
(def face-idx      {:e 0 :w 1 :n 2 :s 3 :f 4 :b 5})

(defn op-args-or-default
  [id node default]
  (:args (if (= id (:op node)) node default)))

(defn node-at
  [tree path]
  (if (seq path) (get-in tree (mg/child-path path)) tree))

(defn num-children-at
  [tree path]
  (count (:out (node-at tree path))))

(defn delete-node-at
  [tree path]
  (assoc-in tree (mg/child-path path) nil))

(defn node-operator
  [n] (cond (:op n) (:op n), n :leaf, :else :delete))

(defn select-sub-paths
  [coll root]
  (let [croot (count root)]
    (->> (keys coll)
         (reduce
          (fn [acc k]
            (if (> (count k) croot)
              (if (every? #(= (nth % 0) (nth % 1)) (partition 2 (interleave root k)))
                (conj acc k)
                acc)
              acc))
          [])
         (select-keys coll))))

(defn select-direct-children
  [coll root]
  (let [croot (inc (count root))]
    (->> (keys coll)
         (reduce
          (fn [acc k]
            (if (= (count k) croot)
              (if (every? #(= (nth % 0) (nth % 1)) (partition 2 (interleave root k)))
                (conj acc k)
                acc)
              acc))
          [])
         (select-keys coll))))

(defn delete-branch-meshes
  [gl meshes root incl-root?]
  (let [d-meshes (select-sub-paths meshes root)
        d-meshes (if incl-root? (conj d-meshes [root (meshes root)]) d-meshes)
        meshes (apply dissoc meshes (keys d-meshes))]
    (dorun
     (map
      (fn [[id m]]
        (.deleteBuffer gl (:buffer m))) d-meshes))
    meshes))

(defn compute-tree-depth
  [nodes]
  (->> nodes keys (map count) (reduce max) (inc)))

(defn init-tree-with-seed
  [state seed-id]
  (let [tree (mg/subdiv :cols 5 :out {1 (mg/subdiv-inset :dir :z :inset 0.2)})
        tree (mg/reflect-seq [:e :e :e :e :e])
        ;;tree {}
        nodes (mg/compute-tree-map (:seed (config/seeds (keyword seed-id))) tree)]
    (merge
     state
     {:tree tree
      :node-cache nodes
      :meshes {}
      :seed-id seed-id
      :selection nil
      :tree-depth (compute-tree-depth nodes)
      :max-nodes-path [1 1 1 1]})))

(defn filter-leaves-and-selection
  [coll tree sel]
  (->> coll
       (filter
        (fn [[path]]
          (or (= path sel) (= :leaf (mg/classify-node-at tree path)))))
       (into {})))

(defn update-meshes
  [state incl-sel?]
  (let [{:keys [gl tree node-cache selection meshes]} state
        path (or selection [])
        root (get node-cache path)
        sub-tree (node-at tree path)
        meshes (delete-branch-meshes gl meshes path incl-sel?)
        branch (select-sub-paths node-cache path)
        node-cache (apply dissoc node-cache (keys branch)) ;; delete all sub-paths
        branch (->> path
                    (mg/compute-tree-map* root sub-tree (transient {}))
                    (persistent!))
        meshes (->> branch
                    (reduce
                     (fn [acc [path node]]
                       (assoc!
                        acc path
                        (-> (g/into (bm/basic-mesh) (g/faces node))
                            (gl/as-webgl-buffer-spec {:tessellate true :fnormals true})
                            (buf/make-attribute-buffers-in-spec gl gl/static-draw))))
                     (transient meshes))
                    (persistent!))
        node-cache (merge node-cache branch)
        selection (if-not (and incl-sel? (nil? sub-tree)) selection)]
    (debug :ct (keys node-cache) :meshes (keys meshes))
    (merge
     state
     {:node-cache node-cache
      :meshes meshes
      :selection selection
      :display-meshes (filter-leaves-and-selection meshes tree selection)})))
