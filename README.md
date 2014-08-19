# DevArt Co(de)Factory

![CodeFactory @ Barbican](assets/codefactory.jpg)

## Build requirements

* Google AppEngine 1.9.8
* Java 1.7
* Leiningen 2.4.0
* Clojure 1.6.0
* ClojureScript 0.0-2280
* lein-cljsbuild 1.0.3
* Grunt 0.4.5

## Running the project locally

'''bash
# download & install AppEngine dependencies into local /lib dir
./install-dependencies.sh

# build & compress HTML & CSS
grunt less:prod htmlmin:prod replace

# compile ClojureScript/JS app
lein do cljsbuild clean, cljsbuild once prod

# launch AppEngine dev server
./launch.sh
'''
