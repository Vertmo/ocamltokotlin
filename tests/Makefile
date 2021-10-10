##
# OcamlToKotlin Tests
#

TESTS := \
	hello.ml \
	functions.ml \
	lambda.ml \
	adt.ml

KTs := $(TESTS:.ml=.kt)
JARs := $(TESTS:.ml=.jar)

all: $(JARs)

%.kt: %.ml
	../_build/default/compiler/mltokotlin.exe $^ > $@

%.jar: %.kt
	kotlinc $^ -include-runtime -d $@

clean:
	rm -rf *.kt *.jar META-INF

# end
