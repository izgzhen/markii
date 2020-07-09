# TODO: reduce code duplication

test01:
	cd tests/test01; ./gradlew assembleDebug
	mkdir -p /tmp/test01.facts
	./build-run-markii.sh tests/test01/app/build/outputs/apk/debug/app-debug.apk /tmp/test01.facts
	python3 tests/compare_facts.py /tmp/test01.facts tests/test01.facts

# The APK is built from tempodroid/tree/c647d51/case-studies/ValidRec
test-validrec:
	mkdir -p /tmp/validrec.facts
	./build-run-markii.sh tests/validrec/app-debug.apk /tmp/validrec.facts
	python3 tests/compare_facts.py /tmp/validrec.facts tests/validrec.facts

test-all: test01 test-validrec

record-all:
	./build-run-markii.sh tests/test01/app/build/outputs/apk/debug/app-debug.apk tests/test01.facts
	./build-run-markii.sh tests/validrec/app-debug.apk tests/validrec.facts

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