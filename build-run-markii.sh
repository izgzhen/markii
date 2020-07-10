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

cd $DIR
if [ -z "$BATCH_RUN" ]; then
    ./markii b
fi
./markii a -p $APK_PATH -clientParam output:$OUTPUT_PATH
