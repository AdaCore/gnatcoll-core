/*----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

#define _GNU_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <signal.h>

typedef long long int sint_64;

/* The following functions are used to implement
 * GNATCOLL.OS.Process.Wait_For_Processes. Basically the functions are used to
 * map SIGCHLD signal to write operations in pipes controlled by the user. */

/* Max index number that can be used in MONITORING_FDS. */
static const int MONITORING_FDS_MAX = 255;

/* Table containing the list of file descriptors that should be notified
 * whenever a SIGCHLD is emited. */
static       int MONITORING_FDS[256];

/* Latest used index in MONITORING_FDS. By construction
 * MONITORING_FDS_LAST + 1 is the next free enlement. */
static       int MONITORING_FDS_LAST = -1;

/* Register a new fd to be notified. Return 0 if OK and -1 otherwise.
 * Note that call to that function is not thread safe. */
int __gnatcoll_add_monitoring_fd (int fd)
{
   if (MONITORING_FDS_LAST < MONITORING_FDS_MAX)
   {
      MONITORING_FDS_LAST++;
      MONITORING_FDS[MONITORING_FDS_LAST] = fd;
      return 0;
   }
   else
   {
      return -1;
   }
}

/* Unregister a file descriptor. Return -1 if the file description cannot
 * be found and 0 otherwise.
 * Note that call to that function is not thread safe. */
int __gnatcoll_remove_monitoring_fd (int fd)
{
   if (fd < 0 || MONITORING_FDS_LAST < 0) { return -1; };

   for (int i = 0; i<= MONITORING_FDS_LAST; i++)
   {
      if (fd == MONITORING_FDS[i])
      {
         if (i < MONITORING_FDS_LAST) {
            MONITORING_FDS[i] = MONITORING_FDS[MONITORING_FDS_LAST];
         }
         MONITORING_FDS_LAST--;
         return 0;
      }
   }
   return -1;
}

/* Subtract two struct timeval values: result = a - b */
void
subtract_timeval (struct timeval *result, const struct timeval *a,
                  const struct timeval *b)
{
   result->tv_sec = a->tv_sec - b->tv_sec;
   result->tv_usec = a->tv_usec - b->tv_usec;
   if (result->tv_usec < 0)
      {
         result->tv_sec -= 1;
         result->tv_usec += 1000000;
      }
}

/* Add two struct timeval values: result = a + b */
void
add_timeval (struct timeval *result, const struct timeval *a,
             const struct timeval *b)
{
   result->tv_sec = a->tv_sec + b->tv_sec;
   result->tv_usec = a->tv_usec + b->tv_usec;
   if (result->tv_usec >= 1000000)
      {
         result->tv_sec += result->tv_usec / 1000000;
         result->tv_usec = result->tv_usec % 1000000;
      }
}

/* wait for write operation on a given file descriptor. */
int
__gnatcoll_wait_for_sigchld (int fd, struct timeval *timeout)
{
   fd_set fd_list;
   int retval = 0;
   char buf[1];
   struct timeval now = { 0, 0 };
   struct timeval end_time = { 0, 0 };

   if (timeout != NULL)
      {
         gettimeofday (&now, NULL);
         add_timeval (&end_time, &now, timeout);
      }

   FD_ZERO (&fd_list);
   FD_SET (fd, &fd_list);

   while (timeout == NULL || timeout->tv_sec > 0
          || (timeout->tv_sec == 0 && timeout->tv_usec > 0))
      {
         retval = select (fd + 1, &fd_list, NULL, NULL, timeout);
         if (retval > 0)
            {
               read (fd, buf, 1);
               return 0;
            }

         if (timeout != NULL)
            {
               gettimeofday (&now, NULL);

               // The select function may or may not update the timeout value;
               // this behavior is not portable. Therefore, we manually update
               // the timeout after each iteration.

               subtract_timeval (timeout, &end_time, &now);
            }
      }
   return -1;
}

/* Handler for SIGCHLD. */
static void __gnatcoll_write_on_sigchld (int signum)
{
   for (int i = 0; i<= MONITORING_FDS_LAST; i++)
   {
      write(MONITORING_FDS[i], "a", 1);
   }
}

/* Set SIGCHLD handler. */
int __gnatcoll_init_sigchld_monitoring()
{
   struct sigaction action;
   action.sa_handler = __gnatcoll_write_on_sigchld;
   sigemptyset(&action.sa_mask);
   action.sa_flags = SA_RESTART;
   return sigaction (SIGCHLD, &action, NULL);
}

/* Check process state.
 * Given a pid return whether the process:
 *    * has terminated (-2),
 *    * is in a waitable state (-1),
 *    * is still running (0)
 * */
int __gnatcoll_process_state(int pid)
{
   siginfo_t infop;
   int result;
   infop.si_pid = 0;
   result = waitid (P_PID, pid, &infop, WEXITED | WNOHANG | WNOWAIT);
   if (result == -1) { return -2; };
   if (infop.si_pid != 0) { return -1; } else { return 0; };
}
