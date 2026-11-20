# pip-tui completion plan

This plan brings pip-tui from its 2016 pre-alpha state to a usable
widget library. Plain TUI elements come first; multimedia comes last.
`TODO.org` keeps the original per-widget design notes.

## State as of 2016

**Working (on Guile 2.0 / guile-ncurses 1.7):**
- Framework: events (`event.scm`), actions and action maps, the main
  loop (`tui-action.scm`), clickable `[bracketed]` regions
  (`hotspot.scm`).
- Rendering helpers: `render-lib.scm` (backgrounds, borders, padding,
  wrapped text, bars, highlights), `string-lib.scm` (text width,
  wrapping, tabs), `border.scm` (six box styles), the green palette,
  right-to-left text via fribidi (`bidi.c`).
- Widgets with demos: Label, Progress Bar, Terminal, Menu Bar.
- Multimedia: PNG loader, image-to-block-art converter
  (`pip-image.scm`), PulseAudio tone and noise generator.

**Known problems:**
- Build lists were incomplete, and one `piptui.so` linked every C
  library, so the plain label demo needed libpulse and libpng.
- The umbrella module `pip-tui.scm` imported the deleted
  `(pip-tui menu)`.
- `list-menu.scm` was only started and does not compile.
- Every widget receives every event; there is no keyboard focus.
  Action return values meant to stop propagation are ignored. Resize
  events have no handler. The main loop sleeps in 1 ms steps.
- Label, Menu Bar, Progress Bar and Terminal each repeat ~150 lines of
  identical property and render boilerplate.
- No tests.

## Phase 0: Get it building again

- [x] Port the build to Guile 3.0.
- [x] Split the C code: core `piptui` (colours, bidi) and separate
      `piptui-png` and `piptui-pulseaudio` extensions, so the TUI
      builds without multimedia libraries.
- [x] Fix the Makefile source lists, make `load-extension` names
      consistent, add an `uninstalled-env` script.
- [x] Fix the umbrella module `(pip-tui pip-tui)`.
- [x] Add `srfi-64` unit tests for the screen-free modules
      (`string-lib`, `hotspot`, `data-lib`, `coords`, `action`,
      `action-map`) and wire them to `make check`. Fix the `coords`
      bugs.
- [ ] Build against guile-ncurses 3.x and run the four existing demos,
      fixing whatever the port broke. (Needs guile-ncurses installed;
      see "Status of Phase 0" below.)

## Phase 1: Framework fixes that later widgets depend on

- [ ] Keyboard focus and event routing: a focused widget, Tab /
      Shift-Tab to move focus, actions returning `#t` stop propagation.
- [ ] Shared widget base: panel, border, padding, colours, alignment,
      show/hide, move, resize. Rebuild the four existing widgets on it.
- [ ] Resize handling: deliver resize events so each widget re-lays
      itself out.
- [ ] Main loop: wait on input with a timeout instead of the fixed
      1 ms sleep; keep idle events for animation.

## Phase 2: Plain TUI widgets, in dependency order

1. [ ] List Menu (finish `list-menu.scm` on the ncurses menu library):
       an unbordered scrolling list with ↑/↓ overflow markers, and a
       bordered fixed-height list with disabled items.
2. [ ] Alphabetized List Menu (List Menu with sorting on).
3. [ ] Description Menu: item and short description on one line.
4. [ ] Info Menu: list on the left, wrapped long description of the
       selected item on the right (reuses Label).
5. [ ] Button widget, then Message Dialog: wrapped, centred text in a
       frame with one or two buttons, highlighted on hover or focus.
6. [ ] Notebook (tabs): shows the panel group of the chosen tab and
       hides the rest.
7. [ ] Text Entry: multi-line editing with OK / Cancel.
8. [ ] Form: built from a simple Scheme description of the fields on
       the ncurses form library, returning the values as an alist.
9. [ ] Demo shell: one Pip-Boy-style app using Notebook, Menu Bar,
       lists, Label and Progress Bar.

## Phase 3: Support libraries that are not multimedia

- [ ] GPS position: gpsd JSON over TCP port 2947 in plain Scheme; a
      widget shows latitude and longitude.
- [ ] Database query: a thin wrapper (guile-sqlite3) plus a widget that
      shows results in an Info or Description Menu.

## Phase 4: Multimedia (last)

- [ ] Image widget: wrap `pip-image` and the PNG loader on the shared
      widget base; test dithering and palettes.
- [ ] Sound effects: make audio optional, verify under PipeWire's
      PulseAudio compatibility, wire up the sounds described in
      `TODO.org` (terminal clicks, drive-grind startup, button hover
      and click).
- [ ] Streaming audio: run an external player (`mpv` or `ogg123`) as a
      child process and control it, rather than reviving
      guile-gstreamer.

## Phase 5: Applications

- [ ] Retro-futurist blog engine (needs Form, Text Entry, database).
- [ ] Streaming music player (needs streaming audio).
- [ ] Security camera control (needs Image).

## Status of Phase 0

Building and testing:

```sh
make            # core + TUI modules (TUI needs guile-ncurses 3.x)
make core       # only the modules that do not need guile-ncurses
make media      # PNG and PulseAudio extensions and modules
make image      # image renderer (needs tui and media)
make check      # unit tests (need only `make core`)
./uninstalled-env guile examples/tui-label-demo.scm
```

Done and verified under Guile 3.0.9:
- `make core`, `make media` and `make check` (5 test files, all
  passing). The PNG reader decodes `images/small_gaucho.png`; the
  PulseAudio module loads and reports `failed` cleanly when no sound
  server is running.
- `coords` and `event` reference `(ncurses curses)` lazily, so they and
  `action-map` load without guile-ncurses and are unit tested.
- Bugs fixed along the way: `coords-zero-area?` used `and` instead of
  `or`; `string-trim-right-to-width` counted codepoints instead of
  cells; `data-lib` used `uniform-vector?`, removed in Guile 3;
  `bidi.c` leaked three buffers per call; `pulseaudio.c` did not
  compile (missing headers, undefined `PIP_API`, a call to a
  never-written `func`) and wrote debug files `wave.txt` / `noise.txt`
  every 100 tones.
- Every `.scm` file except `list-menu.scm` (Phase 2) reads without
  syntax errors.

Still open:
- The TUI tier (`make tui`), `make image` and the demos have not been
  compiled or run, because guile-ncurses could not be installed in
  the environment where the port was done. The next step is to install
  guile-ncurses 3.x, run `make && make image`, and run each demo with
  `./uninstalled-env`.
