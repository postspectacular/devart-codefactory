(ns codefactory.config
  (:require
   [thi.ng.validate.core :as v]))

(def app
  {:author "Karsten Schmidt"
   :title-prefix "Co(de)Factory : "
   :angular {:module "codefactory" :controller "MainController"}
   :includes {:css    ["/css/bootstrap.min.css"
                       "/css/main.css"
                       "//fonts.googleapis.com/css?family=Abel"]
              :js     ["/js/lib/angular.js"
                       "/js/lib/angular-route.js"
                       "/js/lib/angular-animate.js"
                       "/js/lib/angular-strap.js"
                       "/js/lib/angular-strap.tpl.js"
                       "/js/app.js"]
              :js-ie9 ["https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"
                       "https://oss.maxcdn.com/respond/1.4.2/respond.min.js"]}
   :tracking ["UA-51939449-1" "devartcodefactory.com"]
   :video {:aspect 1.777777
           :width 1280
           :height 720
           :formats [{:type "video/webm" :ext ".webm"}
                     {:type "video/mp4" :ext ".mp4"}]}
   :operators [{:icon "split.svg" :label "split"}
               {:icon "inset.svg" :label "inset"}
               {:icon "mirror.svg" :label "mirror"}
               {:icon "ext.svg" :label "pull"}
               {:icon "tilt.svg" :label "tilt"}
               {:icon "scale.svg" :label "scale"}
               {:icon "shift.svg" :label "shift"}
               {:icon "delete.svg" :label "delete"}]

   :validators
   {:api {:new-object {"tree"   [(v/required)
                                 (v/max-length (* 16 1024))]
                       "title"  [(v/min-length 3 (constantly "Untitled"))
                                 (v/max-length 16 (fn [_ v] (subs v 0 16)))]
                       "author" [(v/min-length 3 (constantly "Anonymous"))
                                 (v/max-length 16 (fn [_ v] (subs v 0 16)))]
                       "parent" [(v/optional (v/uuid4))]}
          :get-object {:id [(v/uuid4)]}}}})
