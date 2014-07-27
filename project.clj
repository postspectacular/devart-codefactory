(defproject com.postspectacular/codefactory "0.1.0-SNAPSHOT"
  :description "Google DevArt Co(de)Factory"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src-gae"]

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/core.async "0.1.301.0-deb34a-alpha"]

                 [ring/ring-core "1.2.2"]
                 [compojure "1.1.8"]
                 [hiccup "1.0.5"]
                 [camel-snake-kebab "0.1.5"]
                 [simple-time "0.1.1"]

                 [thi.ng/geom-core "0.3.0-SNAPSHOT"]
                 [thi.ng/geom-types "0.3.0-SNAPSHOT"]
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
                 [com.google.appengine/appengine-api-1.0-sdk "1.9.5"]
                 [com.google.appengine/appengine-api-labs "1.9.5"]
                 [com.google.appengine/appengine-tools-sdk "1.9.5"]
                 [com.google.appengine/appengine-remote-api "1.9.5"]
                 [com.google.appengine/appengine-api-stubs "1.9.5"]
                 [com.google.appengine/appengine-testing "1.9.5"]

                 ;; CLJS
                 [thi.ng/geom-webgl "0.3.0-SNAPSHOT"]]

  :profiles {:dev {:dependencies [[org.clojure/clojurescript "0.0-2246"]]
                   :plugins [[lein-cljsbuild "1.0.3"]
                             ;;[lein-cooper "0.0.1"]
                             ]}}

  :cljsbuild {:builds
              [{:source-paths ["src-cljs"]
                :id "dev"
                :compiler
                {:pretty-print true
                 :output-to "war/staging/js/app.js"
                 :optimizations :whitespace}
                :jar false}
               {:source-paths ["src-cljs"]
                :id "stage"
                :compiler
                {:pretty-print false
                 :output-to "war/staging/js/app.js"
                 :optimizations :advanced}
                :jar false}
               {:source-paths ["src-cljs"]
                :id "prod"
                :compiler
                {:pretty-print false
                 :output-to "war/js/app.js"
                 :optimizations :advanced}
                :jar false}]}

  :aot [thi.ng.gae.servlet])
