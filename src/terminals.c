
#ifdef _WIN32
#include <windows.h>
#include <wincon.h>
#else
#include <unistd.h>
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
   return 0;  //  Unix only
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
   write(forStderr ? 2 : 1, "\r", 1);
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
   write(forStderr ? 2 : 1, "\033[0K", 4);
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
    struct winsize w;
    ioctl(forStderr ? 1 : 0, TIOCGWINSZ, &w);
    return w.ws_col;
#endif
}
