(ns codefactory.config
  (:require
   [clojure.edn :as edn]
   [thi.ng.validate.core :as v]
   [thi.ng.gae.util :as util]))

(def query-result-limit 50)

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

(def app
  {:author       "Karsten Schmidt"
   :title-prefix "Co(de)Factory : "
   :includes {:css    ["/css/app.css"
                       "//fonts.googleapis.com/css?family=Abel"]
              :js     ["/js/app.js"]
              :js-ie9 ["https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"
                       "https://oss.maxcdn.com/respond/1.4.2/respond.min.js"]}
   :tracking ["UA-51939449-1" "devartcodefactory.com"]
   :video {:aspect 1.777777
           :width 1280
           :height 720
           :formats [{:type "video/webm" :ext ".webm"}
                     {:type "video/mp4" :ext ".mp4"}]}
   :operators [{:icon "split.svg" :label "split"}
               {:icon "inset.svg" :label "inset"}
               {:icon "mirror.svg" :label "mirror"}
               {:icon "ext.svg" :label "pull"}
               {:icon "tilt.svg" :label "tilt"}
               {:icon "scale.svg" :label "scale"}
               {:icon "shift.svg" :label "shift"}
               {:icon "delete.svg" :label "delete"}]

   :validators
   {:api {:new-object
          {"tree"     [(v/required)
                       (v/max-length (* 16 1024))
                       (valid-tree)]
           "title"    [(v/min-length 3 (constantly "Untitled"))
                       (v/max-length 16 (fn [_ v] (subs v 0 16)))]
           "author"   [(v/min-length 3 (constantly "Anonymous"))
                       (v/max-length 16 (fn [_ v] (subs v 0 16)))]
           "location" [(v/optional (v/max-length 16))]
           "parent"   [(v/optional (v/uuid4))]}

          :get-object {:id [(v/required) (v/uuid4)]}

          :query-objects
          {"limit" [(v/optional (v/number (fn [_ v] (util/parse-int v 25))))
                    (v/optional (v/in-range 1 query-result-limit))]
           "offset" [(v/optional (v/number (fn [_ v] (util/parse-int v 0))))]}

          :new-job {"object-id" [(v/required) (v/uuid4)]}}}

   :db
   {:query-result-limit query-result-limit
    :public-job-keys :*
    :public-codetree-keys [:id :parent-id :tree :seed
                           :title :author :created
                           :short-uri :preview-uri :stl-uri]}

   :google {:api-key ""}})
