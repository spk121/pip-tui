/*
bidi.h

Copyright 2016 Free Software Foundation, Inc.

This file is part of GNU Guile-Ncurses.

Guile-Ncurses is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-Ncurses is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with Guile-Ncurses.  If not, see
<http://www.gnu.org/licenses/>.
*/

#ifndef BIDI_H
#define BIDI_H

#include "visibility.h"

BIDI_API SCM bidi_string_logical_to_visual (SCM str, SCM alignment);
BIDI_API SCM bidi_get_par_direction (SCM str);

BIDI_API void bidi_init (void);
#endif
