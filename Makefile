SHELL=/bin/sh

# The Package identifiers
export PACKAGE = pip-tui
export PACKAGE_BUGREPORT = mike@lonelycactus.com
export PACKAGE_NAME = pip-tui
export PACKAGE_STRING = pip-tui 0.1
export PACKAGE_TARNAME = pip-tui
export PACKAGE_URL = http://www.lonelycactus.com/software/piptui/
export PACKAGE_VERSION = 0.1

################################################################
# EXECUTABLES

export MKDIR = mkdir
export GUILD:=guild
export GUILE = guile
export GUILE_EFFECTIVE_VERSION=3.0
export INSTALL = /usr/bin/install --verbose --compare
export INSTALL_DATA = ${INSTALL} --mode=644
export INSTALL_PROGRAM = ${INSTALL}

################################################################
# STANDARD INSTALL DIRECTORY VARIABLES

# DESTDIR is prepended to each installed target file
# e.g. $(DESTDIR)$(bindir)/foo
# Do not specify DESTDIR in the makefile at all

export prefix = /usr/local
export exec_prefix = ${prefix}
export bindir = ${exec_prefix}/bin
export sbindir = ${exec_prefix}/sbin
export libexecdir = ${exec_prefix}/libexec
export datarootdir = ${prefix}/share
export datadir = ${datarootdir}
export sysconfdir = ${prefix}/etc
export sharedstatedir = ${prefix}/com
export localstatedir = ${prefix}/var
export includedir = ${prefix}/include
export docdir = ${datarootdir}/doc/${PACKAGE}
export infodir = ${datarootdir}/info
export pdfdir = ${docdir}
export lispdir = ${datarootdir}/emacs/site-lisp
export localedir = ${datarootdir}/locale

export pkgincludedir = $(includedir)/${PACKAGE}
export pkglibdir = $(libdir)/${PACKAGE}

################################################################
# NON-STANDARD INSTALL DIRECTORY VARIABLES

# Installed .scm scheme libraries go here
export guilesitedir :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --variable=sitedir)
export pkgguilesitedir = ${guilesitedir}/${PACKAGE}

# Compiled libguile-*.so extention libraries go here
export guileextensiondir :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --variable=extensiondir)

# Compiled .go scheme libraries go here
export guileobjectdir :=\
 $(shell $(GUILE) -c "(display (%site-ccache-dir))")
export pkgguileobjectdir = $(guileobjectdir)/${PACKAGE}

################################################################
# SOURCES

AUX = Makefile README.md PLAN.md TODO.org uninstalled-env

all:
	$(MAKE) -C pip-tui all

# Only the modules that do not need guile-ncurses.
core:
	$(MAKE) -C pip-tui core

tui:
	$(MAKE) -C pip-tui tui

# PNG and PulseAudio extensions.
media:
	$(MAKE) -C pip-tui media

# Image renderer (needs both tui and media).
image:
	$(MAKE) -C pip-tui image

install-media:
	$(MAKE) -C pip-tui install-media

install: all
	$(MAKE) -C pip-tui install

TAGS:
	$(MAKE) -C pip-tui TAGS

.PHONY: all core tui media image install install-media uninstall clean check dist TAGS

# install-html:
# install-dvi:
# install-pdf:
# install-ps:

uninstall:
	$(MAKE) -C pip-tui uninstall

#install-strip:

clean:
	$(MAKE) -C pip-tui clean

# distclean:

# mostlyclean:

# maintainer-clean:

info:

dvi:

html:

pdf:

ps:

dist:
	echo $(PACKAGE)-$(PACKAGE_VERSION) > .fname
	-rm -rf `cat .fname`
	mkdir `cat .fname`
	mkdir `cat .fname`/pip-tui `cat .fname`/tests `cat .fname`/examples `cat .fname`/images
	cp --verbose pip-tui/*.scm pip-tui/*.[ch] pip-tui/Makefile `cat .fname`/pip-tui
	cp --verbose tests/*.scm `cat .fname`/tests
	cp --verbose examples/*.scm `cat .fname`/examples
	cp --verbose images/* `cat .fname`/images
	cp --verbose $(AUX) `cat .fname`
	tar chzf `cat .fname`.tgz `cat .fname`
	-rm -rf `cat .fname` .fname

check:
	$(MAKE) -C pip-tui check

installcheck:

