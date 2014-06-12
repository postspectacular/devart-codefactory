(ns codefactory.model
  (:require
   [thi.ng.gae.services
    [user :as user]
    [datastore :as ds :refer [defentity]]]))

(defentity User [name email] :key :email)
(defentity CodeTree [id title user ^:clj tree has-stl?] :key :id)
