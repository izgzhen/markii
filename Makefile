test01:
	cd tests/test01; ./gradlew assembleDebug
	mkdir -p /tmp/facts
	./build-run-markii.sh tests/test01/app/build/outputs/apk/debug/app-debug.apk /tmp/facts

JAVA_FILES := $(shell find src -name "*.java")
SCALA_FILES := $(shell find src -name "*.scala")

JARFILE := target/scala-2.13/markii-assembly-0.1.jar

$(JARFILE): $(JAVA_FILES) $(SCALA_FILES)
	sbt assembly

jar: $(JARFILE)

gendoc:
	sbt doc
	rm -rf docs/api
	cp -r target/scala-2.13/api docs
	pandoc README.md -o docs/index.html