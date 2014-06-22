(ns codefactory.controllers.editor
  (:require
   [codefactory.config :as config]
   [codefactory.protocols :as proto]
   [thi.ng.cljs.appstate :as app]
   [thi.ng.cljs.dom :as dom]
   [thi.ng.cljs.io :as io]))

(deftype EditorController
    [^:mutable shared state]
  proto/PLifeCycle
  (init [_ s]
    (prn :init-editor)
    (set! shared s))
  (release [_]
    (prn :release-editor)))

(def instance (EditorController. nil nil))
