/*----------------------------------------------------------------------------
--                                  G N A T C O L L                         --
--                                                                          --
--                     Copyright (C) 2014-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

#ifdef _WIN32
#include <windows.h>
#include <wincon.h>
#include <io.h>
#else
#include <unistd.h>

#ifdef HAVE_TERMIOS_H
#include <termios.h>    // for TIOCGWINSZ on some systems
#endif

#include <sys/ioctl.h>
#include <stdio.h>
#endif

int gnatcoll_get_console_screen_buffer_info(int forStderr) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      return csbiInfo.wAttributes;
   }
#else
   return -1;
#endif
}

void gnatcoll_set_console_text_attribute(int forStderr, int attrs) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   SetConsoleTextAttribute (handle, (WORD)attrs);
#endif
}

int gnatcoll_terminal_has_colors(int fd) {
#ifdef _WIN32
   return _isatty(fd);
#else
   //  Ideally, we should check the terminfo database and check the
   //  max_colors fields (from the command line, this is done with
   //  "tput colors"). However, this is fairly complex, and would
   //  drag in the curses library.
   //  For now, let's just assume that a tty always supports colors,
   //  which is true in this day and age for interactive terminals on
   //  all Unix platforms. A pipe will return 0 below, so will not have
   //  colors by default.
   //  ??? We could also check the value of the TERM environment variable,
   //  but this is very approximate at best.

   return isatty(fd);
#endif
}

void gnatcoll_beginning_of_line(int forStderr) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      csbiInfo.dwCursorPosition.X = 0;
      SetConsoleCursorPosition(handle, csbiInfo.dwCursorPosition);
   }
#else
   //  struct winsize ws;
   //  ioctl(forStderr ? 2 : 1, TIOCGWINSZ, &ws);
   if (write(forStderr ? 2 : 1, "\r", 1) != 1) {
      // Ignore failure for now
   }
#endif
}

void gnatcoll_clear_to_end_of_line(int forStderr) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      DWORD numberOfCharsWritten;
      FillConsoleOutputCharacter(
            handle, ' ',
            csbiInfo.dwSize.X - csbiInfo.dwCursorPosition.X + 1, // length
            csbiInfo.dwCursorPosition, // dWriteCoord
            &numberOfCharsWritten);
   }

#else
   if (write(forStderr ? 2 : 1, "\033[0K", 4) != 4) {
      // Ignore failure for now
   }
#endif
}

int gnatcoll_terminal_width(int forStderr) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;
   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      return (int)csbiInfo.dwSize.X;
   }
   return -1;

#else
#ifdef TIOCGWINSZ
    struct winsize w;
    ioctl(forStderr ? 1 : 0, TIOCGWINSZ, &w);
    return w.ws_col;
#else
    return -1;
#endif
#endif
}
