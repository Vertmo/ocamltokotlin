##
# OcamlToKotlin Tests
#

TESTS := \
	hello.ml \
	functions.ml \
	lambda.ml

KTs := $(TESTS:.ml=.kt)
JAR := $(TESTS:.ml=.jar)

all: $(KTs)

%.kt: %.ml
	../_build/default/compiler/mltokotlin.exe $^ > $@

%.jar: %.kt
	kotlinc $^ -include-runtime -d $@

clean:
	rm -rf *.kt *.jar META-INF

# end
