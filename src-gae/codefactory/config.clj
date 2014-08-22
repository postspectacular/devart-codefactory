(ns codefactory.config
  (:require
   [codefactory.validate :as cv]
   [codefactory.geom :as geom]
   [thi.ng.validate.core :as v]
   [thi.ng.gae.util :as util]))

(def mime-types
  {:edn    "application/edn"
   :json   "application/json"
   :png    "image/png"
   :jpg    "image/jpeg"
   :svg    "image/svg+xml"
   :text   "text/plain"
   :stl    "application/sla"
   :binary "application/octet-stream"})

(def api-mime-types
  (vals (select-keys mime-types [:edn :json])))

(def api-prefix "/api/1.0")

(def api-sign-key "") ;; NOGIT

(def query-result-limit 50)

(def app
  {:lux     {:width 480
             :height 360
             :initial-view [0 0.85 2 0]
             :fov 60
             :margin 0.2
             :halt-spp 100}

   :preview {:width 480
             :height 360
             :initial-view [0 0.85 2 0]
             :fov 45
             :margin 0.2
             :attribs {}
             :shader  {:fill         "#999999"
                       :stroke       "white"
                       :stroke-width 0.5}}

   :video {:aspect 1.777777
           :width 1280
           :height 720
           :formats [{:type "video/webm" :ext ".webm"}
                     {:type "video/mp4" :ext ".mp4"}]}

   :validators
   {:api  {:new-object
           {"tree"     [(v/required)
                        (v/max-length (* 16 1024))
                        (cv/valid-tree)]
            "title"    [(v/min-length 3 (constantly "Untitled"))
                        (v/max-length 16 (fn [_ v] (subs v 0 16)))]
            "author"   [(v/min-length 3 (constantly "Anonymous"))
                        (v/max-length 16 (fn [_ v] (subs v 0 16)))]
            "seed"     [(v/member-of (set (map name (keys geom/seeds))))]
            "location" [(v/optional (v/max-length 16))]
            "parent"   [(v/optional (v/uuid4))]}

           :update-object
           {"id"     [(v/required) (v/uuid4)]
            "status" [(v/member-of #{"approved" "unapproved"})]
            "sig"    [(v/fixed-length 64)]}

           :get-object
           {:id [(v/required) (v/uuid4)]}

           :query-objects
           {"limit"  [(v/optional (v/number (fn [_ v] (util/parse-int v 0))))
                     (v/optional (v/in-range 1 query-result-limit))]
            "offset" [(v/optional (v/number (fn [_ v] (util/parse-int v 0))))
                      (v/optional (v/greater-than -1))]
            "filter" [(v/member-of #{"approved" "unapproved" "all"})]
            "include-ast" [(v/optional (v/boolean (fn [_ v] (not= v "false"))))]}

           :exec-task
           {"sig"   [(v/fixed-length 64)]
            "task"  [(v/member-of #{"process-object"
                                    "regenerate-assets"
                                    "delete-simple-objects"})]}
           :new-job
           {"object-id" [(v/required) (v/uuid4)]}}

    :tasks {:regen-assets
            {"since" [(v/optional (v/number (fn [_ v] (util/parse-long v 0))))]
             "until" [(v/optional (v/number (fn [_ v] (util/parse-long v 0))))]
             "limit" [(v/optional (v/number (fn [_ v] (util/parse-int v 0))))]}

            :delete-simple-objects
            {"since"     [(v/optional (v/number (fn [_ v] (util/parse-long v 0))))]
             "min-depth" [(v/number (fn [_ v] (util/parse-int v 0))) (v/in-range 2 4)]}}}

   :db
   {:query-result-limit query-result-limit
    :public-job-keys :*
    :public-codetree-keys [:id :parent-id :status
                           :tree :tree-depth :seed
                           :title :author :created
                           :short-uri :preview-uri :stl-uri]}

   :storage {:scheme :http
             :bucket "media.devartcodefactory.com"}

   :google {:api-key "" ;; NOGIT
            :tracking ["UA-51939449-1" "devartcodefactory.com"]}})
