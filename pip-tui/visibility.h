/*
  visibility.h

  Copyright 2010, 2016 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef VISIBILITY_H
#define VISIBILITY_H

#if defined _WIN32 || defined __CYGWIN__
#define BIDI_HELPER_DLL_IMPORT __declspec(dllimport)
#define BIDI_HELPER_DLL_EXPORT __declspec(dllexport)
#define BIDI_HELPER_DLL_LOCAL
#else
#if __GNUC__ >= 4
#define BIDI_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
#define BIDI_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
#define BIDI_HELPER_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#else
#define BIDI_HELPER_DLL_IMPORT
#define BIDI_HELPER_DLL_EXPORT
#define BIDI_HELPER_DLL_LOCAL
#endif
#endif

#ifdef BIDI_DLL		 /* defined if BIDI is compiled as a DLL */
#ifdef BIDI_DLL_EXPORTS	 /* defined if we are building the BIDI DLL */
#define BIDI_API BIDI_HELPER_DLL_EXPORT
#else
#define BIDI_API BIDI_HELPER_DLL_IMPORT
#endif /* BIDI_DLL_EXPORTS */
#define BIDI_LOCAL BIDI_HELPER_DLL_LOCAL
#else /* BIDI_DLL is not defined: this means BIDI is a static lib. */
#define BIDI_API
#define BIDI_LOCAL
#endif

#endif

