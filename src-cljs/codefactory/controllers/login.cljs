(ns codefactory.controllers.login
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.app :as app :refer [emit handle-event]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]))

(defn check-login
  [queue]
  (fn
    [e]
    (let [p (.-value (dom/by-id "password"))]
      (if (= p (:pwd config/cookie))
        (emit queue :login nil)
        (emit queue :login-fail nil))
      (.preventDefault e))))

(defmethod handle-event :login
  [_ _ _]
  (.set (goog.net.Cookies. js/document) (:name config/cookie) true (* 48 60 60))
  (route/set-route! "home"))

(defmethod handle-event :login-fail
  [_ _ _]
  (dom/add-class! (dom/by-id "bt-login") "flash-error")
  (js/setTimeout (fn [] (dom/remove-class! (dom/by-id "bt-login") "flash-error")) 800))

(deftype LoginController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-home)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (swap! state assoc :dom-listeners [["#login-form" "submit" (check-login queue)]])
    (app/add-listeners (:dom-listeners @state))
    (.focus (dom/by-id "password")))
  (release [_]
    (debug :release-home)
    (app/remove-listeners (:dom-listeners @state))))

(def instance (LoginController. (atom {}) nil nil))
