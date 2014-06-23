(ns codefactory.controllers.editor
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.appstate :as app]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]
   [thi.ng.geom.ui.arcball :as arcball]))

(defn init-model
  [ctrl id]
  (if id
    (io/request
     :uri (str "/api/models/" id)
     :method :get
     :edn? true
     :success (fn [_ data]
                (proto/update-state ctrl
                              {:model {:uuid id
                                       :seed-id (:seed data)
                                       :tree (:tree data)}}))
     :error (fn [status body]
              (prn :xhr-error status body)))
    ))

(deftype EditorController
    [state ^:mutable shared]
  proto/PLifecycle
  (init [_ s]
    (prn :init-editor)
    (set! shared s)
    (reset! state {})
    (app/listen-state-change! state :uuid [:model :uuid] (fn [_ _ id] (prn :new-uuid id)))
    (init-model _ (get-in @shared [:route :params :id])))
  (release [_]
    (prn :release-editor)
    (set! shared nil))
  proto/PState
  (get-state
    [_] state)
  (update-state
    [_ new]
    (app/merge-state state new)))

(def instance (EditorController. (app/make-state {}) nil))
