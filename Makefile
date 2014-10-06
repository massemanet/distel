PACKAGE := distel
VERSION := $(shell git describe --tags 2> /dev/null)

prefix      = /usr/local
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
datadir     = ${prefix}/share
infodir     = ${prefix}/info
erlc        = erlc
emacs       = emacs

ELISP_DIR = ${datadir}/distel/elisp
EBIN_DIR = ${datadir}/distel/ebin
ERL_SRC_DIR = ${datadir}/distel/src

########################################
## Main part

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

ELISP_SRC := elisp/erlext.el elisp/mcase.el elisp/net-fsm.el elisp/epmd.el \
	elisp/erl.el elisp/derl.el
ELISP_OBJ := $(patsubst %.el,%.elc,${ELISP_SRC})

DOC_SRC  := doc/distel.texi
INFO_OBJ := doc/distel.info
PS_OBJ   := doc/distel.ps

OBJECTS := ${ERL_OBJ} ${ELISP_OBJ} ${C_OBJ} ${INFO_OBJ} ${PS_OBJ}

base: ebin ${ERL_OBJ} ${ELISP_OBJ} ${C_OBJ}
info: ${INFO_OBJ}
postscript: ${PS_OBJ}
all: base info postscript
ebin:
	mkdir ebin

erl: ${ERL_OBJ}

########################################
## Rules
.PHONY: release release_patch release_minor release_major

release: release_patch

release_major:
	./release.sh major

release_minor:
	./release.sh minor

release_patch:
	./release.sh patch

## Erlang
ebin/%.beam: src/%.erl
	${erlc} -W -o ebin +debug_info $<

## Elisp
elisp/%.elc: elisp/%.el
	${emacs} -batch -L elisp -f batch-byte-compile $<

## Info documentation
doc/distel.info: ${DOC_SRC}
	makeinfo -o $@ $<

## Postscript documentation
doc/distel.ps: doc/distel.dvi
	dvips -o $@ $<

doc/distel.dvi: ${DOC_SRC}
	(cd doc; texi2dvi distel.texi)

########################################

clean:
	-rm -f ${OBJECTS} 2>/dev/null

distclean: clean
	-rm -f *~ */*~ 2>/dev/null

install: base
	@echo "* Installing Emacs Lisp Library"
	install -m 775 -d ${ELISP_DIR} ${EBIN_DIR} ${ERL_SRC_DIR}
	install -m 775 elisp/*.el elisp/*.elc ${ELISP_DIR}
	@echo
	@echo "* Installing Erlang Library"
	install -m 775 ebin/*.beam ${EBIN_DIR}
	install -m 775 src/*.erl ${ERL_SRC_DIR}
	@echo
	@echo "*** Successfully installed. See README for usage instructions."
	@echo

info_install: info
	  @echo "* Installing Info documentation"
	  cp doc/distel.info ${infodir}
# NB: Debian's not-GNU-compatible install-info needs "--section Emacs Emacs"
	  install-info --info-dir=${infodir} --section Emacs \
		       ${infodir}/distel.info

dist: always distclean
	cd .. && ln -sf ${PACKAGE} ${PACKAGE}-${VERSION}
	cd .. && (find ${PACKAGE}-${VERSION} -follow -type f | \
		  egrep -v '(^attic/)|/CVS/|\.cvsignore|\.rej|\.orig|\#' | \
		  xargs tar czf ${PACKAGE}-${VERSION}.tar.gz)
	rm ../${PACKAGE}-${VERSION}

wc:
	@echo "* Emacs Lisp"
	@wc -l */*.el | sort -nr
	@echo "* Erlang"
	@wc -l */*.erl | sort -nr
	@echo "* C"
	@wc -l */*.c | sort -nr

.INTERMEDIATE: doc/distel.dvi
.PHONY: always
