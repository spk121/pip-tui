SHELL=/bin/sh

# The Package identifiers
PACKAGE = pip-tui
PACKAGE_BUGREPORT = mike@lonelycactus.com
PACKAGE_NAME = pip-tui
PACKAGE_STRING = pip-tui 0.0
PACKAGE_TARNAME = pip-tui
PACKAGE_URL = http://www.lonelycactus.com/software/piptui/
PACKAGE_VERSION = 0.1


# Use only the necessary suffixes
# .SUFFIXES:
# .SUFFIXES: .c .o .scm .go

builddir = .
srcdir = src

################################################################
# EXECUTABLES

# $(AR) $(BISON) $(CC) $(FLEX) $(INSTALL) $(LD) $(LDCONFIG) $(LEX)
# $(MAKE) $(MAKEINFO) $(RANLIB) $(TEXI2DVI) $(YACC)
MKDIR = mkdir
GUILD = guild
GUILE = guile
GUILE_EFFECTIVE_VERSION = 2.0
INSTALL = /usr/bin/install --verbose --compare
INSTALL_DATA = ${INSTALL} --mode=644
INSTALL_PROGRAM = ${INSTALL}

################################################################
# FLAGS

# preprocessor
CPPFLAGS := -DPIC -I${srcdir}\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --cflags-only-I)\
 $(shell pkg-config fribidi --cflags-only-I)

# C compiler
CFLAGS := -g -O2 -fPIC\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --cflags-only-other)\
 $(shell pkg-config fribidi --cflags-only-other)

# SCM compiler
SCMFLAGS := -Wformat -Wunbound-variable -Warity-mismatch -L $(srcdir)

# Linker flags
LDFLAGS := -shared -fPIC\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --libs-only-L --libs-only-other)\
 $(shell pkg-config fribidi --libs-only-L --libs-only-other)

LIBS :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --libs-only-l)\
 $(shell pkg-config fribidi --libs-only-l)

################################################################
# STANDARD INSTALL DIRECTORY VARIABLES

# DESTDIR is prepended to each installed target file
# e.g. $(DESTDIR)$(bindir)/foo
# Do not specify DESTDIR in the makefile at all

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datarootdir = ${prefix}/share
datadir = ${datarootdir}
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
includedir = ${prefix}/include
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
infodir = ${datarootdir}/info
pdfdir = ${docdir}
lispdir = ${datarootdir}/emacs/site-lisp
localedir = ${datarootdir}/locale

pkgincludedir = $(includedir)/piptui
pkglibdir = $(libdir)/piptui

################################################################
# NON-STANDARD INSTALL DIRECTORY VARIABLES

# Installed .scm scheme libraries go here
guilesitedir :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --variable=sitedir)
pkgguilesitedir = ${guilesitedir}/${PACKAGE}

# Compiled libguile-*.so extention libraries go here
guileextensiondir :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --variable=extensiondir)

# Compiled .go scheme libraries go here
guileobjectdir :=\
 $(shell $(GUILE) -c "(display (cadr %load-compiled-path))")
pkgguileobjectdir = $(guileobjectdir)/${PACKAGE}

################################################################
# SOURCES

# All the C source is in srcdir
libguile_fribidi_SOURCES = \
 $(srcdir)/bidi.c
libguile_fribidi_HEADERS = \
 $(srcdir)/bidi.h \
 $(srcdir)/visibility.h
libguile_fribidi_OBJECTS = \
 $(builddir)/bidi.o

# All the Guile source is in $(srcdir)/pip-tui/*.scm
# We need that pip-tui subdir so that we test the code
# in tree.
# The Guile objects should go into $(builddir)/pip-tui/*.go
SCM_SOURCES = \
 $(srcdir)/pip-tui/string-lib.scm \
 $(srcdir)/pip-tui/fribidi.scm \
 $(srcdir)/pip-tui/unistring.scm

SCM_OBJECTS = \
 $(builddir)/pip-tui/string-lib.go \
 $(builddir)/pip-tui/fribidi.go \
 $(builddir)/pip-tui/unistring.go

AUX = GNUmakefile README.md

all: $(SCM_OBJECTS) libguile-fribidi.so

# Any binary libraries have to exist before Guile files
# can be compiled.
$(SCM_OBJECTS): libguile-fribidi.so

# Build the C library
libguile-fribidi.so: $(libguile_fribidi_OBJECTS)
	$(CC) $(LDFLAGS) $^ $(LIBS) -o $@

# The local .go files go here
localbuilddir:
	mkdir -p $(builddir)/pip-tui

# The local .go files need a directory
$(SCM_OBJECTS): | localbuilddir

$(builddir)/pip-tui/%.go: $(srcdir)/pip-tui/%.scm
	unset GUILE_LOAD_COMPILED_PATH ; LC_ALL=C \
	LD_LIBRARY_PATH=$(builddir) $(GUILD) compile $(SCMFLAGS) -o "$@" "$<"

$(builddir)/bidi.o: $(srcdir)/bidi.c $(srcdir)/bidi.h $(srcdir)/visibility.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

install: installdirs all
	$(INSTALL_DATA) $(SCM_SOURCES) $(DESTDIR)$(pkgguilesitedir)
	$(INSTALL_DATA) $(SCM_OBJECTS) $(DESTDIR)$(pkgguileobjectdir)
	$(INSTALL_DATA) libguile-fribidi.so $(DESTDIR)$(guileextensiondir)

# install-html:
# install-dvi:
# install-pdf:
# install-ps:

uninstall:
	-rm -f $(DESTDIR)$(pkgguilesitedir)/*.scm
	-rm -f $(DESTDIR)$(pkgguileobjectdir)/*.go
	-rm -f $(DESTDIR)$(guileextensiondir)/libguile-fribidi.so
	-rmdir $(DESTDIR)$(pkgguilesitedir)
	-rmdir $(DESTDIR)$(pkgguileobjectdir)

#install-strip:

clean:
	-rm -f $(builddir)/libguile-fribidi.so
	-rm -f $(libguile_fribidi_OBJECTS)
	-rm -f $(SCM_OBJECTS)
	-rmdir $(builddir)/pip-tui

# distclean:

# mostlyclean:

# maintainer-clean:

TAGS: $(libguile_fribidi_SOURCES) $(libguile_fribidi_HEADERS) $(SCM_SOURCES)
	etags -o $@ $^

info:

dvi:

html:

pdf:

ps:

dist: $(libguile_fribidi_SOURCES) $(libguile_fribidi_HEADERS) $(SCM_SOURCES) $(AUX)
	echo $(PACKAGE)-$(PACKAGE_VERSION) > .fname
	-rm -rf `cat .fname`
	mkdir `cat .fname`
	cp --verbose $(libguile_fribidi_SOURCES) $(libguile_fribidi_HEADERS) `cat .fname`
	mkdir `cat .fname`/pip-tui
	cp --verbose $(SCM_SOURCES) `cat .fname`/pip-tui
	cp --verbose $(AUX) `cat .fname`
	tar chzf `cat .fname`.tgz `cat .fname`
	-rm -rf `cat .fname` .fname

check:

installcheck:

installdirs:
	for dir in "$(DESTDIR)$(pkgguilesitedir)" "$(DESTDIR)$(pkgguileobjectdir)" "$(DESTDIR)$(guileextensiondir)"; do \
	  test -z "$$dir" || $(MKDIR) -p "$$dir"; \
	done
