#!/bin/bash

set -x
set -e

sblf=$1
base=$(basename -s .nuscr $sblf)

stack build
stack exec -- HsDottyGen-exe -f $sblf -ev
sudo rm -f ../DottyGen/Orig/effpi_sandbox/src/main/scala/*.scala
sudo cp ./scala/$base.scala ../DottyGen/Orig/effpi_sandbox/src/main/scala
docker exec `docker ps -q -l` sbt "tests/runMain effpi_sandbox.$base.Main"

