(ns codefactory.model
  (:require
   [thi.ng.gae.services
    [user :as user]
    [datastore :as ds :refer [defentity]]]))

(defentity CodeTree
  [id title author
   ^:clj tree
   stl-uri
   video-uri
   preview-uri
   created
   stl-created
   video-created
   preview-created
   parent-id]
  :key :id)
