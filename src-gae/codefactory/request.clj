(ns codefactory.request
  (:require
   [codefactory.config :as config]
   [thi.ng.gae.util :as util]
   [ring.util.codec :as codec]))

(defn request-signature
  [uri params]
  (->> config/api-sign-key
       (str uri ";" (codec/form-encode params) ";" )
       (util/sha-256)))
