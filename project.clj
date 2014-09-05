(defproject com.postspectacular/codefactory "0.1.0-SNAPSHOT"
  :description "Google DevArt Co(de)Factory"
  :url "https://github.com/postspectacular/devart-codefactory"
  :license {:name "Apache Software License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  :source-paths ["src-gae"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]

                 [ring/ring-core "1.2.2"]
                 [compojure "1.1.8"]
                 [hiccup "1.0.5"]
                 [camel-snake-kebab "0.1.5"]
                 [simple-time "0.1.1"]

                 [thi.ng/geom-core "0.3.0-SNAPSHOT"]
                 [thi.ng/geom-types "0.3.0-SNAPSHOT"]
                 [thi.ng/geom-meshops "0.3.0-SNAPSHOT"]
                 [thi.ng/geom-svg "0.3.0-SNAPSHOT"]
                 [thi.ng/luxor "0.3.0-SNAPSHOT"]
                 [thi.ng/morphogen "0.1.0-SNAPSHOT"]
                 [thi.ng/macromath "0.2.1"]
                 [thi.ng/validate "0.1.0-SNAPSHOT"]

                 ;; GAE
                 [javax.servlet/servlet-api "2.5"]
                 [commons-io "2.4"]
                 [commons-codec "1.9"]
                 [commons-fileupload "1.3.1"]

                 ;; GAE admin
                 [tomcat/jasper-runtime "5.5.23"]
                 [org.apache.geronimo.specs/geronimo-jsp_2.1_spec "1.0.1"]
                 [javax.servlet/jstl "1.1.2"]
                 [taglibs/standard "1.1.2"]
                 [commons-el "1.0"]

                 ;; GAE standard libs
                 [com.google.appengine/appengine-api-1.0-sdk "1.9.8"]
                 [com.google.appengine/appengine-api-labs "1.9.8"]
                 [com.google.appengine/appengine-tools-sdk "1.9.8"]
                 [com.google.appengine/appengine-remote-api "1.9.8"]
                 [com.google.appengine/appengine-api-stubs "1.9.8"]
                 [com.google.appengine/appengine-testing "1.9.8"]
                 [com.google.appengine.tools/appengine-gcs-client "0.4" :exclusions [commons-logging]]
                 ;;[com.google.apis/google-api-services-compute "v1-rev29-1.18.0-rc" :exclusions [commons-logging]]

                 ;; CLJS
                 [thi.ng/geom-webgl "0.3.0-SNAPSHOT"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2322"]
                                  [clj-http "1.0.0"]]
                   :plugins [[lein-cljsbuild "1.0.3"]]}}
  
  :cljsbuild {:builds
              [{:source-paths ["src-cljs"]
                :id "dev"
                :compiler
                {:pretty-print true
                 :output-to "war/staging/js/app.js"
                 :optimizations :whitespace}
                :jar false}
               {:source-paths ["src-cljs"]
                :id "prod"
                :compiler
                {:pretty-print false
                 :output-to "war/js/app.js"
                 :optimizations :advanced}
                :jar false}]}

  :aot :all)
