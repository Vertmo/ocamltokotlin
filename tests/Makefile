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

%.kt: %.ml printingLib.cmi
	../_build/default/compiler/mltokotlin.exe $< > $@

%.cmi: %.ml
	../_build/default/compiler/mltokotlin.exe $< > /dev/null

%.jar: %.kt
	kotlinc $^ -include-runtime -d $@

clean:
	rm -rf *.cmi *.kt *.jar META-INF

# end
