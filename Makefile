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
	@ls -1 R/*  | tr '\n' ' ' >> sources.txt
	@echo "" >> sources.txt
	@echo -n "DATAFILES=" >> sources.txt
	@[ -d data ] && ls -1 data/*  | tr '\n' ' ' >> sources.txt
	@echo ""

${PACKAGE_TARGET}: NAMESPACE DESCRIPTION .Rbuildignore version.txt
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

.PHONY: prepclean doclean checkclean targetclean reallyclean

prepclean:
	rm -f NAMESPACE sources.txt version.txt man/*.Rd

checkclean:
	rm -rf ${PACKAGE_NAME}.Rcheck

doclean:
	rm -f ${DOCFILE}

targetclean:
	rm -f ${PACKAGE_TARGET}

clean: doclean checkclean targetclean

reallyclean: clean prepclean
