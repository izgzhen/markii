#!/bin/bash
# This script will mount the current source directory to docker and build the
# artifact incrementally before running on the target APK

set -e
set -x

if ! command -v sbt &> /dev/null
then
    echo "sbt could not be found. Install it here -- https://www.scala-sbt.org/1.x/docs/Setup.html"
    exit
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

$DIR/check-jdk-version

APK_PATH=$(realpath $1)
OUTPUT_PATH=$(realpath $2)
if [ ! -z "$API_SEMANTICS_CONFIG" ]; then
    API_SEMANTICS_CONFIG_PATH=$(realpath $API_SEMANTICS_CONFIG)
fi

cd $DIR
if [ -z "$BATCH_RUN" ]; then
    ./markii b
fi
if [ -z "$API_SEMANTICS_CONFIG" ]; then
    ./markii a -p $APK_PATH -clientParam output:$OUTPUT_PATH
else
    ./markii a -p $APK_PATH -clientParam output:$OUTPUT_PATH \
        -clientParam apiSemanticConfig:$API_SEMANTICS_CONFIG_PATH
fi