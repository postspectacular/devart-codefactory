(ns codefactory.gallery
  (:require
   [codefactory.config :as config]
   [thi.ng.geom.webgl.core :as gl]))

(def controller-id "GalleryController")

(def module-spec
  {:controllers
   [{:id controller-id
      :spec #js ["$scope" "$routeParams"
                 (fn [$scope $routeParams]
                   (prn :init "GalleryController" $routeParams))]}]})
