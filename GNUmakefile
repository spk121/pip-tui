SHELL=/bin/sh

# The Package identifiers
PACKAGE = pip-tui
PACKAGE_BUGREPORT = mike@lonelycactus.com
PACKAGE_NAME = pip-tui
PACKAGE_STRING = pip-tui 0.0
PACKAGE_TARNAME = pip-tui
PACKAGE_URL = http://www.lonelycactus.com/software/piptui/
PACKAGE_VERSION = 0.1


builddir = .
srcdir = .

################################################################
# EXECUTABLES

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
 $(shell pkg-config fribidi --cflags-only-I) \
 $(shell pkg-config libpng --cflags-only-I)

# C compiler
CFLAGS := -g -O2 -fPIC\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --cflags-only-other)\
 $(shell pkg-config fribidi --cflags-only-other) \
 $(shell pkg-config libpng --cflags-only-other)


# SCM compiler
SCMFLAGS := -Wformat -Wunbound-variable -Warity-mismatch -L $(srcdir)

# Linker flags
LDFLAGS := -shared -fPIC\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --libs-only-L --libs-only-other)\
 $(shell pkg-config fribidi --libs-only-L --libs-only-other) \
 $(shell pkg-config libpng --libs-only-L --libs-only-other)

LIBS :=\
 $(shell pkg-config guile-$(GUILE_EFFECTIVE_VERSION) --libs-only-l)\
 $(shell pkg-config fribidi --libs-only-l) \
 $(shell pkg-config libpng --libs-only-l)

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

VPATH = pip-tui
LIB_SOURCES = bidi.c png.c
LIB_HEADERS = bidi.h png.h visibility.h
LIB_OBJECTS = $(patsubst %.c,%.o,$(LIB_SOURCES))

# All the Guile source is in $(srcdir)/pip-tui/*.scm
# We need that pip-tui subdir so that we test the code
# in tree.
# The Guile objects should go into $(builddir)/pip-tui/*.go
SCM_SOURCES = string-lib.scm fribidi.scm unistring.scm tui-label.scm \
 pip-colors.scm pip-color-names.scm
SCM_OBJECTS = $(patsubst %.scm,%.go,$(SCM_SOURCES)))

AUX = GNUmakefile README.md

all: $(SCM_OBJECTS) piptui.so

# Any binary libraries have to exist before Guile files
# can be compiled.
$(SCM_OBJECTS): piptui.so

# Build the C library
piptui.so: $(LIB_OBJECTS)
	$(CC) $(LDFLAGS) $^ $(LIBS) -o $@

# The local .go files go here
localbuilddir:
	mkdir -p $(builddir)/pip-tui

# The local .go files need a directory
$(SCM_OBJECTS): | localbuilddir

%.go: %.scm
	unset GUILE_LOAD_COMPILED_PATH ; LC_ALL=C \
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(builddir) $(GUILD) compile $(SCMFLAGS) -o $@ $<

bidi.o: bidi.c bidi.h visibility.h

png.o: png.c png.h visibility.h

install: installdirs all
	$(INSTALL_DATA) $(SCM_SOURCES) $(DESTDIR)$(pkgguilesitedir)
	$(INSTALL_DATA) $(SCM_OBJECTS) $(DESTDIR)$(pkgguileobjectdir)
	$(INSTALL_DATA) piptui.so $(DESTDIR)$(guileextensiondir)

# install-html:
# install-dvi:
# install-pdf:
# install-ps:

uninstall:
	-rm -f $(DESTDIR)$(pkgguilesitedir)/*.scm
	-rm -f $(DESTDIR)$(pkgguileobjectdir)/*.go
	-rm -f $(DESTDIR)$(guileextensiondir)/piptui.so
	-rmdir $(DESTDIR)$(pkgguilesitedir)
	-rmdir $(DESTDIR)$(pkgguileobjectdir)

#install-strip:

clean:
	-rm -f $(builddir)/piptui.so
	-rm -f $(LIB_OBJECTS)
	-rm -f $(SCM_OBJECTS)
	-rmdir $(builddir)/pip-tui

# distclean:

# mostlyclean:

# maintainer-clean:

TAGS: $(LIB_SOURCES) $(LIB_HEADERS) $(SCM_SOURCES)
	etags -o $@ $^

info:

dvi:

html:

pdf:

ps:

dist: $(LIB_SOURCES) $(LIB_HEADERS) $(SCM_SOURCES) $(AUX)
	echo $(PACKAGE)-$(PACKAGE_VERSION) > .fname
	-rm -rf `cat .fname`
	mkdir `cat .fname`
	cp --verbose $(LIB_SOURCES) $(LIB_HEADERS) `cat .fname`
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
