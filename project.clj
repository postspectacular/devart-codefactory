(defproject com.postspectacular/codefactory "0.1.0-SNAPSHOT"
  :description "Google DevArt Co(de)Factory"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.1.8"]
                 [ring/ring-core "1.2.2"]
                 [hiccup "1.0.2"]
                 [camel-snake-kebab "0.1.5"]
                 [javax.servlet/servlet-api "2.5"]
                 [commons-io "2.4"]
                 [commons-codec "1.9"]
                 [commons-fileupload "1.3.1"]
                 ;; App Engine admin
                 [tomcat/jasper-runtime "5.5.23"]
                 [org.apache.geronimo.specs/geronimo-jsp_2.1_spec "1.0.1"]
                 [javax.servlet/jstl "1.1.2"]
                 [taglibs/standard "1.1.2"]
                 [commons-el "1.0"]
                 ;; App Engine libraries
                 [com.google.appengine/appengine-api-1.0-sdk "1.9.5"]
                 [com.google.appengine/appengine-api-labs "1.9.5"]
                 [com.google.appengine/appengine-tools-sdk "1.9.5"]
                 [com.google.appengine/appengine-remote-api "1.9.5"]
                 [com.google.appengine/appengine-api-stubs "1.9.5"]
                 [com.google.appengine/appengine-testing "1.9.5"]]

  :dev {:dependencies [[org.clojure/clojurescript "0.0-2246"]
                       [purnam "0.1.0-beta"]]
        :plugins [[lein-cljsbuild "1.0.3"]]}

  :cljsbuild {:builds
              [{:source-paths ["src-cljs"]
                :id "dev"
                :compiler
                {:pretty-print false
                 :output-to "war/js/app.js"
                 :externs ["war/js/lib/angular.js"]
                 :optimizations :whitespace}
                :jar false}]}

  :aot [thi.ng.gae.servlet])
