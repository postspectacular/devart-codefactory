(ns codefactory.config)

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
           :formats [{:type "video/webm" :ext ".webm"}]}})
