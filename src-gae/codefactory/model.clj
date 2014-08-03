(ns codefactory.model
  (:require
   [codefactory.config :as config]
   [thi.ng.gae.services
    [user :as user]
    [datastore :as ds :refer [defentity]]]))

(defn public-entity
  [e key-id]
  (let [keys (get-in config/app [:db key-id])]
    (if (= :* keys) (into {} e) (select-keys e keys))))

(defentity CodeTree
  [id title author
   ^:clj tree
   seed
   author-location
   short-uri
   stl-uri
   video-uri
   preview-uri
   created
   stl-created
   video-created
   preview-created
   parent-id
   job-id]
  :key :id)

(defentity PrintJob
  [id
   object-id
   status
   created
   started
   user]
  :key :id)

(defentity PrintLog
  [id type date message]
  :key :id)
