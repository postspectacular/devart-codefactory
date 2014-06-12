#!/bin/bash
lein do compile, jar \
  && cp target/codefactory-0.1.0-SNAPSHOT.jar war/WEB-INF/lib/ \
  && dev_appserver.sh war/
