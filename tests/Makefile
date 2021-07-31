##
# OcamlToKotlin Tests
#

TESTS := hello.ml
KTs := $(TESTS:.ml=.kt)

all: $(KTs)

%.kt: %.ml
	../_build/default/compiler/mltokotlin.exe $^ > $@

%.jar: %.kt
	kotlinc $^ -include-runtime -d $@

clean:
	rm -f *.kt

# end
