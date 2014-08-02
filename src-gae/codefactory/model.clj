(ns codefactory.model
  (:require
   [thi.ng.gae.services
    [user :as user]
    [datastore :as ds :refer [defentity]]]))

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
