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

MKDIR = mkdir
GUILE_EFFECTIVE_VERSION ?= 3.0
GUILE ?= guile
GUILD ?= guild
INSTALL = /usr/bin/install --verbose --compare
INSTALL_DATA = ${INSTALL} --mode=644
PKG_CONFIG ?= pkg-config

################################################################
# STANDARD INSTALL DIRECTORY VARIABLES

prefix ?= /usr/local

################################################################
# NON-STANDARD INSTALL DIRECTORY VARIABLES

# Installed .scm scheme libraries go here
guilesitedir :=\
 $(shell $(PKG_CONFIG) guile-$(GUILE_EFFECTIVE_VERSION) --variable=sitedir)
pkgguilesitedir = ${guilesitedir}/${PACKAGE}

# Compiled C extension libraries go here
guileextensiondir :=\
 $(shell $(PKG_CONFIG) guile-$(GUILE_EFFECTIVE_VERSION) --variable=extensiondir)

# Compiled .go scheme libraries go here
guileobjectdir := $(shell $(GUILE) -c "(display (%site-ccache-dir))")
pkgguileobjectdir = $(guileobjectdir)/${PACKAGE}

srcdir = .

################################################################
# FLAGS

GUILE_CFLAGS := $(shell $(PKG_CONFIG) guile-$(GUILE_EFFECTIVE_VERSION) --cflags)
GUILE_LIBS := $(shell $(PKG_CONFIG) guile-$(GUILE_EFFECTIVE_VERSION) --libs)

CPPFLAGS += -I$(srcdir)
CFLAGS ?= -g -O2
CFLAGS += -fPIC -Wall $(GUILE_CFLAGS)
LDFLAGS += -shared -fPIC

# SCM compiler
SCMFLAGS := -Wformat -Wunbound-variable -Warity-mismatch

# Environment used to compile and test Scheme modules from the build tree.
# GUILE_EXTENSIONS_PATH lets load-extension find the freshly built
# piptui*.so files.
SCM_ENV = LC_ALL=C.UTF-8 GUILE_AUTO_COMPILE=0 \
 GUILE_LOAD_PATH="$(abspath ..):$(GUILE_LOAD_PATH)" \
 GUILE_LOAD_COMPILED_PATH="$(abspath ..):$(GUILE_LOAD_COMPILED_PATH)" \
 GUILE_EXTENSIONS_PATH="$(abspath .):$(GUILE_EXTENSIONS_PATH)"

################################################################
# C EXTENSIONS

# Core extension: colour distance and bidi.  Needs fribidi.
CORE_LIB = piptui.so
CORE_C = colors.c bidi.c
CORE_CFLAGS := $(shell $(PKG_CONFIG) fribidi --cflags)
CORE_LIBS := $(shell $(PKG_CONFIG) fribidi --libs) -lm

# Media extensions: built only by `make media`.
PNG_LIB = piptui-png.so
PNG_C = pip_png.c
PNG_CFLAGS = $(shell $(PKG_CONFIG) libpng --cflags)
PNG_LIBS = $(shell $(PKG_CONFIG) libpng --libs)

PULSE_LIB = piptui-pulseaudio.so
PULSE_C = pulseaudio.c
PULSE_CFLAGS = $(shell $(PKG_CONFIG) libpulse --cflags)
PULSE_LIBS = $(shell $(PKG_CONFIG) libpulse --libs) -lm

################################################################
# SCHEME MODULES

# Modules that do not need guile-ncurses to compile or load.
CORE_SCM = \
 typecheck.scm \
 data-lib.scm \
 time.scm \
 unistring.scm \
 fribidi.scm \
 string-lib.scm \
 hotspot.scm \
 coords.scm \
 event.scm \
 action.scm \
 action-map.scm \
 pip-color-names.scm

# Modules that need guile-ncurses.
# list-menu.scm is excluded until it is finished (PLAN.md, Phase 2).
TUI_SCM = \
 border.scm \
 pip-colors.scm \
 render-lib.scm \
 tui-action.scm \
 tui-label.scm \
 tui-progress-bar.scm \
 tui-terminal.scm \
 tui-menubar.scm \
 pip-tui.scm

# Multimedia modules.
MEDIA_SCM = \
 png.scm \
 pulseaudio.scm

# The image renderer needs both the media and the TUI tiers.
IMAGE_SCM = pip-image.scm

CORE_GO = $(CORE_SCM:.scm=.go)
TUI_GO = $(TUI_SCM:.scm=.go)
MEDIA_GO = $(MEDIA_SCM:.scm=.go)
IMAGE_GO = $(IMAGE_SCM:.scm=.go)

################################################################
# TARGETS

all: core tui

core: $(CORE_LIB) $(CORE_GO)

tui: core $(TUI_GO)

media: core $(PNG_LIB) $(PULSE_LIB) $(MEDIA_GO)

image: tui media $(IMAGE_GO)

$(CORE_LIB): $(CORE_C:.c=.o)
	$(CC) $(LDFLAGS) $^ $(GUILE_LIBS) $(CORE_LIBS) -o $@

$(PNG_LIB): $(PNG_C:.c=.o)
	$(CC) $(LDFLAGS) $^ $(GUILE_LIBS) $(PNG_LIBS) -o $@

$(PULSE_LIB): $(PULSE_C:.c=.o)
	$(CC) $(LDFLAGS) $^ $(GUILE_LIBS) $(PULSE_LIBS) -o $@

bidi.o: bidi.c bidi.h visibility.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(CORE_CFLAGS) -c $< -o $@

colors.o: colors.c colors.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

pip_png.o: pip_png.c pip_png.h visibility.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PNG_CFLAGS) -c $< -o $@

pulseaudio.o: pulseaudio.c pulseaudio.h visibility.h
	$(CC) $(CPPFLAGS) $(CFLAGS) $(PULSE_CFLAGS) -c $< -o $@

%.go: %.scm
	$(SCM_ENV) $(GUILD) compile $(SCMFLAGS) -o $@ $<

# Module dependencies, so that imported modules are compiled first.
data-lib.go time.go typecheck.go pip-color-names.go:
unistring.go: typecheck.go
fribidi.go: $(CORE_LIB)
string-lib.go: typecheck.go unistring.go fribidi.go
hotspot.go: string-lib.go
coords.go:
event.go: typecheck.go
action.go: typecheck.go
action-map.go: typecheck.go action.go event.go
border.go:
pip-colors.go: pip-color-names.go $(CORE_LIB)
render-lib.go: border.go coords.go pip-color-names.go pip-colors.go string-lib.go
tui-action.go: typecheck.go pip-color-names.go time.go action-map.go event.go
tui-label.go: string-lib.go border.go coords.go pip-colors.go pip-color-names.go render-lib.go
tui-progress-bar.go: typecheck.go string-lib.go border.go coords.go pip-colors.go pip-color-names.go render-lib.go
tui-terminal.go: typecheck.go time.go data-lib.go string-lib.go hotspot.go coords.go event.go action.go pip-color-names.go render-lib.go tui-action.go
tui-menubar.go: string-lib.go event.go action.go action-map.go tui-action.go data-lib.go border.go coords.go pip-colors.go pip-color-names.go render-lib.go
pip-tui.go: action.go action-map.go event.go tui-action.go border.go pip-colors.go pip-color-names.go tui-label.go tui-progress-bar.go tui-terminal.go tui-menubar.go
png.go:
pulseaudio.go: action.go
pip-image.go: pip-colors.go

check: core
	$(SCM_ENV) $(GUILE) ../tests/run-tests.scm

install: installdirs all
	$(INSTALL_DATA) $(CORE_SCM) $(TUI_SCM) $(DESTDIR)$(pkgguilesitedir)
	$(INSTALL_DATA) $(CORE_GO) $(TUI_GO) $(DESTDIR)$(pkgguileobjectdir)
	$(INSTALL_DATA) $(CORE_LIB) $(DESTDIR)$(guileextensiondir)

install-media: installdirs media image
	$(INSTALL_DATA) $(MEDIA_SCM) $(IMAGE_SCM) $(DESTDIR)$(pkgguilesitedir)
	$(INSTALL_DATA) $(MEDIA_GO) $(IMAGE_GO) $(DESTDIR)$(pkgguileobjectdir)
	$(INSTALL_DATA) $(PNG_LIB) $(PULSE_LIB) $(DESTDIR)$(guileextensiondir)

uninstall:
	-rm -f $(DESTDIR)$(pkgguilesitedir)/*.scm
	-rm -f $(DESTDIR)$(pkgguileobjectdir)/*.go
	-rm -f $(DESTDIR)$(guileextensiondir)/piptui.so
	-rm -f $(DESTDIR)$(guileextensiondir)/piptui-png.so
	-rm -f $(DESTDIR)$(guileextensiondir)/piptui-pulseaudio.so
	-rmdir $(DESTDIR)$(pkgguilesitedir)
	-rmdir $(DESTDIR)$(pkgguileobjectdir)

clean:
	-rm -f *.so *.o *.go

TAGS: $(CORE_C) $(PNG_C) $(PULSE_C) $(CORE_SCM) $(TUI_SCM) $(MEDIA_SCM) $(IMAGE_SCM)
	etags -o $@ $^

installdirs:
	for dir in "$(DESTDIR)$(pkgguilesitedir)" "$(DESTDIR)$(pkgguileobjectdir)" "$(DESTDIR)$(guileextensiondir)"; do \
	  test -z "$$dir" || $(MKDIR) -p "$$dir"; \
	done

.PHONY: all core tui media image check install install-media uninstall clean installdirs
