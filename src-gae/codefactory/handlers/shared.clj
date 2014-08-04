(ns codefactory.handlers.shared
  (:require
   [codefactory.config :as config]))

(defn api-route
  [& args] (apply str config/api-prefix (interpose \/ args)))

(defn server-url
  [{:keys [scheme server-name server-port]} & more]
  (let [base (str (name scheme) "://" server-name)
        base (if (or (== 80 server-port)
                     (== 443 server-port))
               base
               (str base ":" server-port))]
    (apply str base (interpose \/ more))))

(defn storage-url
  [path]
  (let [{:keys [scheme bucket]} (:storage config/app)]
    (str (name scheme) bucket path)))
