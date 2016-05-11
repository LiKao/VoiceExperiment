-include version.txt
-include sources.txt

PACKAGE_TARGET=${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz

DOCFILE=doc/${PACKAGE_NAME}.pdf

all: ${PACKAGE_TARGET}


.PHONY: prepare
prepare: sources.txt version.txt NAMESPACE


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
	@find R -type "f" -printf "%p " >> sources.txt
	@echo "" >> sources.txt
	@echo -n "DATAFILES=" >> sources.txt
	@if [ -d data ]; then find data -type "f" -printf "%p " >> sources.txt; fi
	@echo "" >> sources.txt
	@echo -n "TESTFILES=" >> sources.txt
	@if [ -d tests/testthat/ ]; then find tests/testthat -type "f" -printf "%p " >> sources.txt; fi
	@echo "" >> sources.txt
	@echo -n "TESTDATAFILES=" >> sources.txt
	@if [ -d tests/testdata/ ]; then find tests/testdata/ -type "f" -printf "%p " >> sources.txt; fi
	@echo "" >> sources.txt


${PACKAGE_TARGET}: NAMESPACE DESCRIPTION .Rbuildignore version.txt ${TESTFILES} ${TESTDATAFILES}
	R CMD build .

.PHONY: install
install: ${PACKAGE_TARGET}
	R CMD INSTALL ${PACKAGE_TARGET}

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

.PHONY: test
test:
	@LANG=C R -e "devtools::test('.')"

.PHONY: prepclean doclean checkclean targetclean reallyclean

prepclean:
	rm -f NAMESPACE sources.txt version.txt man/*.Rd

checkclean:
	rm -rf ${PACKAGE_NAME}.Rcheck

doclean:
	rm -f ${DOCFILE}

targetclean:
	rm -f ${PACKAGE_TARGET}

clean: prepclean doclean checkclean targetclean

reallyclean: clean prepclean
