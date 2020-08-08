bindir = ${HOME}/bin

all: covid19

uberscript: src/covid19/babashka/*.clj
	bb --uberscript uberscript --classpath src -m covid19.babashka.core

covid19: uberscript
	echo "#!/usr/bin/env bb\n" > covid19
	cat uberscript >> covid19
	chmod +x covid19

install: covid19
	mkdir -p ${bindir}
	cp -p covid19 ${bindir}

clean:
	rm -rf target uberscript
