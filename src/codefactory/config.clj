(ns codefactory.config
  (:require
   [thi.ng.validate.core :as v]
   [thi.ng.gae.util :as util]))

(def query-result-limit 50)

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
          {"tree"   [(v/required)
                     (v/max-length (* 16 1024))]
           "title"  [(v/min-length 3 (constantly "Untitled"))
                     (v/max-length 16 (fn [_ v] (subs v 0 16)))]
           "author" [(v/min-length 3 (constantly "Anonymous"))
                     (v/max-length 16 (fn [_ v] (subs v 0 16)))]
           "parent" [(v/optional (v/uuid4))]}

          :get-object {:id [(v/uuid4)]}

          :query-objects
          {"limit" [(v/optional (v/number (fn [_ v] (util/parse-int v 25))))
                    (v/optional (v/in-range 1 query-result-limit))]
           "offset" [(v/optional (v/number (fn [_ v] (util/parse-int v 0))))]}}}

   :db
   {:query-result-limit query-result-limit
    :public-codetree-keys [:id :parent-id :title :author :created :preview-uri]}})
