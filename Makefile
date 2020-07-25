# TODO: reduce code duplication

TEST01_APK := tests/test01/app/build/outputs/apk/debug/app-debug.apk

build-test01:
	cd tests/test01; ./gradlew assembleDebug

test-test01: build-test01
	mkdir -p /tmp/test01.facts
	./build-run-markii.sh $(TEST01_APK) /tmp/test01.facts
	python3 tests/compare_facts.py /tmp/test01.facts tests/test01.facts

# The APK is built from tempodroid/tree/c647d51/case-studies/ValidRec
test-validrec:
	mkdir -p /tmp/validrec.facts
	API_SEMANTICS_CONFIG=tests/recorder.yaml ./build-run-markii.sh tests/validrec/app-debug.apk /tmp/validrec.facts
	python3 tests/compare_facts.py /tmp/validrec.facts tests/validrec.facts

test-all: test-test01 test-validrec

record-test01: build-test01
	./build-run-markii.sh $(TEST01_APK) tests/test01.facts

record-validrec:
	API_SEMANTICS_CONFIG=tests/recorder.yaml ./build-run-markii.sh tests/validrec/app-debug.apk tests/validrec.facts

record-all: record-test01 record-validrec

update-testBasic-apk: build-test01
	cp $(TEST01_APK) src/test/resources/testBasic/app-debug.apk
	@echo "TODO: manually copy over res and manifest from first step"

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