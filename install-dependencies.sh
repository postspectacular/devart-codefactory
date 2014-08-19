#!/bin/bash

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(cd "$(dirname "$0")"; pwd)
readonly ARGS="$@"
readonly LIB=$PROGDIR/war/WEB-INF/lib

main() {
    local jars="
appengine-api-1.0-sdk-1.9.8.jar
appengine-api-labs-1.9.8.jar
appengine-gcs-client-0.4.jar
appengine-remote-api-1.9.8.jar
appengine-tools-sdk-1.9.8.jar
camel-snake-kebab-0.1.5.jar
clj-time-0.6.0.jar
clojure-1.6.0.jar
clout-1.2.0.jar
common-0.2.0-SNAPSHOT.jar
commons-codec-1.9.jar
commons-compress-1.4.jar
commons-el-1.0.jar
commons-exec-1.1.jar
commons-fileupload-1.3.1.jar
commons-io-2.4.jar
commons-logging-1.0.3.jar
compojure-1.1.8.jar
core.incubator-0.1.0.jar
data.json-0.2.5.jar
dependency-0.1.1.jar
fs-1.4.5.jar
geom-core-0.3.0-SNAPSHOT.jar
geom-meshops-0.3.0-SNAPSHOT.jar
geom-svg-0.3.0-SNAPSHOT.jar
geom-types-0.3.0-SNAPSHOT.jar
geom-webgl-0.3.0-SNAPSHOT.jar
geronimo-jsp_2.1_spec-1.0.1.jar
google-api-client-1.18.0-rc.jar
google-api-client-appengine-1.18.0-rc.jar
google-api-client-servlet-1.18.0-rc.jar
google-api-services-storage-v1-rev1-1.18.0-rc.jar
google-http-client-1.18.0-rc.jar
google-http-client-appengine-1.18.0-rc.jar
google-http-client-jackson2-1.18.0-rc.jar
google-http-client-jdo-1.18.0-rc.jar
google-oauth-client-1.18.0-rc.jar
google-oauth-client-appengine-1.18.0-rc.jar
google-oauth-client-servlet-1.18.0-rc.jar
guava-15.0.jar
hiccup-1.0.5.jar
httpclient-4.0.1.jar
httpcore-4.0.1.jar
jackson-core-2.1.3.jar
jasper-runtime-5.5.23.jar
jdo2-api-2.3-eb.jar
joda-time-2.3.jar
jstl-1.1.2.jar
luxor-0.3.0-SNAPSHOT.jar
macromath-0.2.1.jar
morphogen-0.1.0-SNAPSHOT.jar
ring-codec-1.0.0.jar
ring-core-1.2.2.jar
servlet-api-2.5.jar
simple-time-0.1.1.jar
standard-1.1.2.jar
tools.logging-0.2.3.jar
tools.macro-0.1.0.jar
tools.reader-0.7.3.jar
tools.reader-0.8.3.jar
tools.trace-0.7.3.jar
transaction-api-1.1.jar
validate-0.1.0-SNAPSHOT.jar
xz-1.0.jar
"
    echo "downloading dependencies..."
    lein deps

    echo "creating /lib directory..."
    mkdir -p $LIB

    for jar in $jars
    do
        local src=`find ~/.m2/repository -name $jar`
        local dest=$LIB/$jar
        echo "copy $src -> $dest"
        cp $src $dest
    done
}

main
