(ns codefactory.validate
  (:require
   [clojure.edn :as edn]
   [thi.ng.validate.core :as v]
   [thi.ng.gae.util :as util]))

(defn validate-params
  [params validators]
  (v/validate params validators))

(defn valid-accept?
  [req types]
  (let [^String accept (:accept (:headers req))]
    (or (= accept "*/*")
        (some #(util/str-contains? accept %) types))))

(defn validate-node
  [node]
  (cond
   (map? node) (let [{:keys [op args out]} node]
                 (if op
                   (and (map? args)
                        (vector? out)
                        (pos? (count out))
                        (every? validate-node out))
                   (nil? (seq node))))
   (nil? node) true
   :else false))

(def valid-tree
  (v/validator
   (fn [_ v]
     (when-let [tree (try (edn/read-string v) (catch Exception e))]
       (and (map? tree) (:op tree) (:out tree) (validate-node tree))))
   "must be a valid operator tree"))
