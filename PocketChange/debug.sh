#!/bin/bash

export MAVEN_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=4000,server=y,suspend=y"

mvn -Djetty.port=9090 jetty:run
