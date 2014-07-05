(ns codefactory.controllers.submit
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.log :refer [debug info warn]]
   [thi.ng.cljs.route :as route]
   [thi.ng.cljs.app :as app :refer [emit handle-event]]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]))

(defn validate-form
  [q]
  (fn
    [e]
    (emit q :submit-fail)
    (.preventDefault e)))

(defmethod handle-event :submit-succeed
  [_ _ _]
  )

(defmethod handle-event :submit-fail
  [_ _ _]
  (dom/add-class! (dom/by-id "bt-login") "flash-error")
  (js/setTimeout (fn [] (dom/remove-class! (dom/by-id "bt-login") "flash-error")) 800))

(defmethod handle-event :submit-cancel
  [_ _ _]
  (route/set-route! "edit" "new" "box"))

(deftype SubmitController
    [state ^:mutable shared ^:mutable queue]
  proto/PController
  (init [_ opts]
    (debug :init-home)
    (set! shared (:state opts))
    (set! queue (:queue opts))
    (swap! state assoc
           :dom-listeners [["#submit-art" "submit" (validate-form queue)]
                           ["#submit-cancel" "click" (fn [e] (emit queue :submit-cancel nil) (.preventDefault e))]])
    (app/add-listeners (:dom-listeners @state))
    (.focus (dom/by-id "password")))
  (release [_]
    (debug :release-home)
    (app/remove-listeners (:dom-listeners @state))))

(def instance (SubmitController. (atom {}) nil nil))
