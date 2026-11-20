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

## Open question: should this be SDL instead of guile-ncurses?

The Pip-Boy in the Fallout games is not really a text terminal. It is
a monochrome raster display pretending to be one: green-on-black with
scanlines and phosphor glow, an animated Vault Boy, line-drawn boxes
that wrap the selected tab (with the tab bar's rule broken around the
selection), small monochrome pictures beside item descriptions, and a
map. The question is whether a terminal is the right medium for that.

### What a terminal can do

More than it first seems.

- **Boxes and tab outlines.** Box-drawing characters already cover
  this. A selected tab is a `┌──┐` above the label and `┘  └` cut into
  the rule below it. `border.scm` has the pieces.
- **Monochrome pictures.** Block characters give sub-cell resolution:
  half blocks give 1×2 pixels per cell, quadrants 2×2 (what
  `pip-image.scm` does now), the Unicode 13 sextants 2×3, and Braille
  2×4. In a single green tint, monochrome is the easy case: 2×4 Braille
  on an 80×24 terminal is 160×96 pixels, about the size of a Vault Boy
  sprite.
- **Simple animation.** The main loop already redraws at 60 fps, and a
  few frames of a Braille or quadrant sprite swapped on idle events
  costs very little. The terminal widget's typing effect is the same
  mechanism.
- **Colour.** Most terminals do 256 colours, and many do 24-bit
  colour, which is enough for a convincing green ramp.

What it cannot do: scanlines, glow, curvature, real fonts, pixel-exact
images, or any graphics finer than a cell grid. Terminal graphics
protocols (sixel, kitty, iTerm2) can draw real bitmaps, but ncurses
doesn't know about them, so mixing the two means fighting ncurses over
who owns the screen, and support varies a lot between terminals.

### What SDL would give

- **Fidelity.** Real CRT effects with a shader pass, real bitmaps,
  smooth sprite animation, any font, and boxes and lines at pixel
  precision. It would actually look like the Pip-Boy.
- **Audio in the same library.** SDL_mixer could replace the
  hand-written PulseAudio engine, and covers sound effects and local
  OGG/MP3 playback. Icecast streams would still need an HTTP fetch
  feeding a decoder, or the external-player approach from Phase 4.
- **Hardware.** The real-world Pip-Boy builds are a Raspberry Pi with
  a small LCD and some buttons, not a terminal. SDL runs fullscreen
  there without X (KMS/DRM) and handles gamepads and rotary encoders.
- **Guile bindings exist.** guile-sdl2 is a reasonably complete
  binding, and Chickadee builds a 2D game toolkit with sprites, fonts
  and shaders on top of it.

### What SDL would cost

- **It stops being a TUI.** No running over SSH, no running inside an
  existing terminal session. Arguably the charm of the project is a
  Pip-Boy you can run in xterm.
- **Text layout becomes our problem.** ncurses (with libunistring)
  handles cell widths, wide CJK characters and combining marks.
  With SDL, text shaping and measurement come from SDL_ttf or
  Chickadee, and bidi and wrapping have to be redone in pixels. Or the
  renderer draws a fixed character grid itself, which is half a
  terminal emulator.
- **Lose ncurses menu and form.** Phase 2 planned to build List Menu
  and Form on the ncurses menu and form libraries; under SDL those are
  hand-written.
- **A rewrite of the drawing layer.** Though that layer is smaller than
  it sounds: there are roughly 60 direct ncurses calls, concentrated in
  `render-lib.scm`, the four widgets and `tui-action.scm`. Most of the
  code (events, actions, hotspots, string wrapping, colour matching,
  image dithering) doesn't touch ncurses at all.

### A middle path: keep the cell grid, abstract the backend

The widgets already think in a grid of character cells. That model
fits both media: ncurses draws the grid in a terminal, and an SDL
backend draws the same grid as glyphs from a bitmap font, plus a
pixel layer for images and sprites and a CRT shader over everything.
The widget code stays the same; the SDL backend just gets to cheat
where the terminal can't. This is how many roguelikes work
(libtcod, BearLibTerminal).

That would change the plan in three places:

1. **Phase 1, shared widget base:** route all drawing through a small
   backend interface (put a styled string at y,x; fill a region; draw a
   box; show, hide and stack a panel; get the next input event)
   instead of calling ncurses from widgets. `render-lib.scm` becomes
   that interface, with an ncurses implementation first.
2. **Phase 2:** write List Menu and Form on pip-tui's own primitives
   rather than the ncurses menu and form libraries, so they work on
   both backends. A bit more work, but it also removes the menu
   library's limits the `list-menu.scm` comments already ran into
   (no custom overflow arrows).
3. **Phase 4:** add the SDL backend (guile-sdl2 or Chickadee) as the
   multimedia step: grid renderer, sprite and image layer, CRT
   shader, SDL_mixer audio. The image widget then draws Braille or
   quadrants on ncurses and real pixels on SDL.

### Recommendation

Don't switch outright. The terminal is part of the project's charm,
and it gets surprisingly close with box-drawing tab outlines and
Braille sprites. But make the backend abstraction part of Phase 1,
before more widgets are written directly against ncurses, so an SDL
backend stays a Phase 4 addition rather than a rewrite. The decision
point is the start of Phase 1: once List Menu and Form are built on
the ncurses menu and form libraries, switching gets much more
expensive.
