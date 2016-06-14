SHELL=/bin/sh

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
 $(shell pkg-config libpng --libs-only-l)

################################################################
# SOURCES

LIB_SOURCES = bidi.c pip_png.c
LIB_HEADERS = bidi.h pip_png.h visibility.h
LIB_OBJECTS = $(patsubst %.c,%.o,$(LIB_SOURCES))

SCM_SOURCES = string-lib.scm fribidi.scm unistring.scm tui-label.scm \
 pip-colors.scm pip-color-names.scm tui-progress-bar.scm border.scm
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
