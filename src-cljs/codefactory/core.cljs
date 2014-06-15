(ns codefactory.core
  (:require
   [thi.ng.angular]
   [codefactory.config :as config]
   [codefactory.home :as home]
   [codefactory.editor :as editor]
   [codefactory.gallery :as gallery])
  (:require-macros [hiccups.core :as h]))

(enable-console-print!)

(defn add-module-spec
  [app spec]
  (doseq [{:keys [id spec]} (:directives spec)]
    (.directive app id spec))
  (doseq [{:keys [id spec]} (:controllers spec)]
    (.controller app id spec))
  app)

(def ^:export app
  (let [app (.. js/angular
                (module config/module-name
                        #js ["mgcrea.ngStrap" "ngRoute" "ngAnimate" "thi.ng.ngSupport"])

                (config
                 #js ["$routeProvider" "$locationProvider"
                      (fn [$routeProvider $locationProvider]
                        (.. $routeProvider
                            (when "/"
                              #js {:controller "HomeController"
                                   :templateUrl "/templates/home"})
                            (when "/edit/:id"
                              #js {:controller editor/controller-id
                                   :template "<webgl-canvas width=\"640\" height=\"480\" fill></webgl-canvas>"})
                            (when "/gallery"
                              #js {:controller gallery/controller-id})
                            (otherwise #js {:redirectTo "/"}))
                        ;;(.html5Mode $locationProvider true)
                        )])

                (controller
                 "MainController"
                 #js ["$scope" "$route" "$routeParams" "$location"
                      (fn [$scope $route $routeParams $location]
                        (prn :init "MainController" $routeParams))]))]

    (-> app
        (add-module-spec home/module-spec)
        (add-module-spec editor/module-spec)
        (add-module-spec gallery/module-spec))))
