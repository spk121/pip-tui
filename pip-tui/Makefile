SHELL=/bin/sh

# The Package identifiers
export PACKAGE = pip-tui
export PACKAGE_BUGREPORT = mike@lonelycactus.com
export PACKAGE_NAME = pip-tui
export PACKAGE_STRING = pip-tui 0.0
export PACKAGE_TARNAME = pip-tui
export PACKAGE_URL = http://www.lonelycactus.com/software/piptui/
export PACKAGE_VERSION = 0.1

################################################################
# EXECUTABLES

export MKDIR = mkdir
export GUILD:=guild
export GUILE = guile
export GUILE_EFFECTIVE_VERSION=2.0
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
 $(shell $(GUILE) -c "(display (cadr %load-compiled-path))")
export pkgguileobjectdir = $(guileobjectdir)/${PACKAGE}

srcdir = .

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
 $(shell pkg-config libpng --libs-only-l) \
 -lm

################################################################
# SOURCES

LIB_SOURCES = bidi.c pip_png.c colors.c
LIB_HEADERS = bidi.h pip_png.h colors.h visibility.h
LIB_OBJECTS = $(patsubst %.c,%.o,$(LIB_SOURCES))

SCM_SOURCES = string-lib.scm fribidi.scm unistring.scm tui-label.scm \
 pip-colors.scm pip-color-names.scm tui-progress-bar.scm border.scm \
 pip-image.scm
SCM_OBJECTS = $(patsubst %.scm,%.go,$(SCM_SOURCES))

all: $(SCM_OBJECTS) piptui.so

# Any binary libraries have to exist before Guile files
# can be compiled.
$(SCM_OBJECTS): piptui.so

# Build the C library
piptui.so: $(LIB_OBJECTS)
	$(CC) $(LDFLAGS) $^ $(LIBS) -o $@

%.go: %.scm
	unset GUILE_LOAD_COMPILED_PATH ; LC_ALL=C \
	GUILE_LOAD_PATH="$(GUILE_LOAD_PATH):.." \
	LD_LIBRARY_PATH="$(LD_LIBRARY_PATH):." \
	GUILE_AUTO_COMPILE=0 \
	$(GUILD) compile $(SCMFLAGS) -o $@ $<

bidi.o: bidi.c bidi.h visibility.h

png.o: png.c png.h visibility.h

colors.o: colors.c colors.h

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
	-rm -f piptui.so
	-rm -f $(LIB_OBJECTS)
	-rm -f $(SCM_OBJECTS)

# distclean:
# mostlyclean:
# maintainer-clean:

TAGS: $(LIB_SOURCES) $(LIB_HEADERS) $(SCM_SOURCES)
	etags -o $@ $^

# info:
# dvi:
# html:
# pdf:
# ps:
# dist:
# check:
# installcheck:

installdirs:
	for dir in "$(DESTDIR)$(pkgguilesitedir)" "$(DESTDIR)$(pkgguileobjectdir)" "$(DESTDIR)$(guileextensiondir)"; do \
	  test -z "$$dir" || $(MKDIR) -p "$$dir"; \
	done
