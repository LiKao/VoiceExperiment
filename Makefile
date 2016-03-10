-include version.txt
-include sources.txt

PACKAGE_TARGET=${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz

DOCFILE=doc/${PACKAGE_NAME}.pdf

all: ${PACKAGE_TARGET}


.PHONY: version
version: version.txt

version.txt:	DESCRIPTION Makefile
	@echo -n "PACKAGE_NAME=" >   version.txt
	@grep "Package:" DESCRIPTION | sed "s/Package: *//" >> version.txt
	@echo -n "PACKAGE_VERSION=" >> version.txt
	@grep "Version:" DESCRIPTION | sed "s/Version: *//" >> version.txt


.PHONY: sources
sources: sources.txt

sources.txt: Makefile
	@echo -n "SOURCEFILES=" > sources.txt
	@ls -1 R/*  | tr '\n' ' ' >> sources.txt
	@echo "" >> sources.txt
	@echo -n "DATAFILES=" >> sources.txt
	@if [ -d data ]; then ls -1 data/*  | tr '\n' ' ' >> sources.txt; fi
	@echo "" >> sources.txt
	@echo -n "TESTFILES=" >> sources.txt
	@if [ -d tests/testthat/ ]; then ls -1 tests/testthat/* | tr '\n' ' ' >> sources.txt; fi
	@echo "" >> sources.txt
	@echo -n "TESTDATAFILES=" >> sources.txt
	@if [ -d tests/testdata/ ]; then ls -1 tests/testdata/* | tr '\n' ' ' >> sources.txt; fi
	@echo "" >> sources.txt


${PACKAGE_TARGET}: NAMESPACE DESCRIPTION .Rbuildignore version.txt ${TESTFILES} ${TESTDATAFILES}
	R CMD build .

NAMESPACE: ${SOURCEFILES} ${DATAFILES}
	R -e 'devtools::document()'
	touch NAMESPACE

.PHONY: doc
doc: ${DOCFILE}

${DOCFILE}: NAMESPACE
	mkdir -p doc
	@R CMD Rd2pdf --batch --no-preview --force . -o ${DOCFILE} > /dev/null 2>&1

.PHONY: check
check: ${PACKAGE_TARGET}
	R CMD check ${PACKAGE_TARGET}

.PHONY: doclean checkclean targetclean

checkclean:
	rm -rf ${PACKAGE_NAME}.Rcheck

doclean:
	rm -f ${DOCFILE}

targetclean:
	rm -f ${PACKAGE_TARGET}

clean: doclean checkclean targetclean
