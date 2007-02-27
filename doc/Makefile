# Interface
infodir = "/usr/local/share/info" # FIXME: configure

all: info postscript

info: distel.info

postscript: distel.ps

install: distel.info
	@echo "* Installing Info documentation"
	cp distel.info ${infodir}
#	Debian's install-info is quirky
	if [ -f /etc/debian_version ]; then \
		install-info --info-dir=${infodir} --section Emacs Emacs \
			distel.info; \
	else \
		install-info --info-dir=${infodir} --section Emacs \
			distel.info; \
	fi

# Helpers
distel.info: distel.texi
	makeinfo -o $@ $<

distel.ps: distel.dvi
	dvips -o $@ $<

distel.dvi: distel.texi
	texi2dvi $<


