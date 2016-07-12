# pip-tui

This project is pre-alpha, and is not usable or stable.

This project is some widgets for a text user interface inspired by the
fictional computer from the Fallout video games, and a basic
application written in those widgets. The widget API is in the GNU
Guile dialect of Scheme.  The code is a mixture of GNU Guile dialect
of Scheme and of C.

To build, the code requires or could someday require
* guile-2.0.x
* guile-ncurses-1.7
* libunistring
* fribidi
* libpng
* pulseaudio
* maybe eventually *gstreamer* 
* maybe eventually *guile-gstreamer* if that can be resurrected
* maybe eventually *guile-mysql* or *guile-db*
* maybe eventually *gpsd*

## TODO
### Widget Containers
- [ ] Notebook: a tabbed notebook container

### Widgets
- [x] Label: (string) auto wrapping text in a box
- [x] Progress Bar: (string/number) a short text and a progress bar
- [x] Terminal: (string) auto wrapping text that reveals itself one
      character at a time. Bracketed text are clickable links.
- [ ] Menu Bar: (alist key string) a small menu, arranged horizontally
      in one line.
- [ ] Message Dialog (string x3): wrapped, centered text in a frame,
      with one or two buttons.
- [ ] Menu: (list) a scrolling list of elements, where one can be selected
- [ ] Description Menu: (alist) a scrolling list of elements and short
      descrpitions
- [ ] Info Menu: (alist) a scrolling list of elements with long descriptions
- [ ] Image: (u32 array) an Unicode-art rendering of an RGB32 array
- [ ] Form: (?) a form with fillable fields
- [ ] Text Entry: (?) a multi-line text entry widget

### Support Libraries
- [ ] Streaming audio playback
- [ ] GPS position
- [ ] Database query

### Applications
- [ ] Streaming music player
- [ ] Security camera control
- [ ] Retro-futurist blog engine

## Thought of the Day

Putting together a main loop with Pulseaudio support was more annoying
than I thought, but, it is cool that I get to recycle some chiptune code.

Guile's GStreamer support is rotten; how will I play Icecast streams?

If I really want to do fallback rendering for Latin1, it would take some effort.
