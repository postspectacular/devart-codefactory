(ns thi.ng.gae.services.user
  (:require
   [thi.ng.gae.services.core :refer [defservice]])
  (:import
   [com.google.appengine.api.users User UserService UserServiceFactory]))

(defservice get-user-service
  *user-service* UserService (UserServiceFactory/getUserService))

(defn ^User current-user []
  (.getCurrentUser (get-user-service)))

(defn logged-in? []
  (.isUserLoggedIn (get-user-service)))

(defn admin? []
  (.isUserAdmin (get-user-service)))

(defn login-url
  [^String redirect]
  (.createLoginURL (get-user-service) redirect))

(defn logout-url
  [^String redirect]
  (.createLogoutURL (get-user-service) redirect))

(defn user-id [^User user]
  (.getUserId user))

(defn user-email [^User user]
  (.getEmail user))

(defn user-nickname [^User user]
  (.getNickname user))

(defn user-auth-domain [^User user]
  (.getAuthDomain user))
