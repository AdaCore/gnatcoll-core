/*----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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
#include "windows.h"
#include "winternl.h"

NTSTATUS
__gnatcoll_ntopenfile (HANDLE *handle,
                       PUNICODE_STRING name,
 ACCESS_MASK desired_access,
 PIO_STATUS_BLOCK io,
 ULONG access,
 ULONG options)
{
  OBJECT_ATTRIBUTES attr;
  NTSTATUS status;

  InitializeObjectAttributes(&attr, name, OBJ_CASE_INSENSITIVE, NULL, NULL);
  status = NtOpenFile (handle,
                       desired_access,
                       &attr,
                       io,
                       access,
                       options);
  return status;
}

/* Implement a simplified wait_for_multiple_objects that can wait for more
   than 64 processes (the limit is 64 * 64 = 4096).
*/
typedef struct WaitParameters {
   DWORD count;
   HANDLE *handles;
   BOOL  wait_all;
   DWORD milliseconds;
   DWORD result;
} WAIT_PARAMETERS;

static DWORD thread_wait_for_multiple_objects(LPVOID lpParam)
{

   WAIT_PARAMETERS *params = (WAIT_PARAMETERS *) lpParam;

   params->result = WaitForMultipleObjects
      (params->count,
       params->handles,
       params->wait_all,
       params->milliseconds);
}

int
__gnatcoll_wait_for_multiple_objects (
      DWORD nCount,
      HANDLE *lpHandles,
      BOOL bWaitAll,
      DWORD dwMilliseconds)
{
   WAIT_PARAMETERS thread_params[64];
   HANDLE threads[64];
   int thread_number = 0;
   int last_thread_size = 0;
   int selected_thread = 0;
   DWORD result;
   DWORD wait_result = 0;

   /* Handle simple case.  */
   if (nCount <= MAXIMUM_WAIT_OBJECTS) {
      wait_result = WaitForMultipleObjects (
         nCount,
         lpHandles,
         bWaitAll,
         dwMilliseconds);
      if (wait_result == WAIT_TIMEOUT) {
         return -2;
      } else if (wait_result == WAIT_FAILED) {
         return -1;
      } else if (wait_result >= WAIT_ABANDONED_0)
      {
         return wait_result - WAIT_ABANDONED_0;
      } else
      {
         return wait_result - WAIT_OBJECT_0;
      }
   }

   /* This is a case for which we need to wait on more than
      MAXIMUM_WAIT_OBJECTS. */
   if (nCount > MAXIMUM_WAIT_OBJECTS * MAXIMUM_WAIT_OBJECTS) {
      return -2;
   }

   /* Compute the number of needed threads.  */
   thread_number = nCount / MAXIMUM_WAIT_OBJECTS;
   last_thread_size = nCount % MAXIMUM_WAIT_OBJECTS;
   if (last_thread_size > 0)
   {
      thread_number++;
   }

   for (int i=0; i<thread_number; i++)
   {
      if (i == thread_number - 1 && last_thread_size > 0) {
         thread_params[i].count = nCount % MAXIMUM_WAIT_OBJECTS;
      } else {
         thread_params[i].count = MAXIMUM_WAIT_OBJECTS;
      }
      thread_params[i].handles = lpHandles + 64 * i;
      thread_params[i].wait_all = bWaitAll;
      thread_params[i].milliseconds = dwMilliseconds;
      threads[i] = CreateThread (NULL,
                                 1,
                                 thread_wait_for_multiple_objects,
                                 &thread_params[i],
                                 0,
                                 NULL);
   }

   /* Wait for a thread to finish or for the timeout. */
   wait_result = WaitForMultipleObjects (
      thread_number,
      threads,
      bWaitAll,
      dwMilliseconds);

   /* Exit all threads.  */
   for (int i=0; i<thread_number; i++)
   {
      TerminateThread(threads[i], 1);
      CloseHandle(threads[i]);
   }

   /* Compute the final result.  */
   /* First find the thread. */
   if (wait_result == WAIT_TIMEOUT) {
      return -2;
   } else if (wait_result == WAIT_FAILED) {
      return -1;
   } else if (wait_result >= WAIT_ABANDONED_0)
   {
      selected_thread = wait_result - WAIT_ABANDONED_0;
   } else
   {
      selected_thread = wait_result - WAIT_OBJECT_0;
   }

   /* Then the status for that thread. */
   result = thread_params[selected_thread].result;
   if (result == WAIT_TIMEOUT) {
      return -2;
   } else if (result == WAIT_FAILED) {
      return -1;
   } else if (result >= WAIT_ABANDONED_0) {
      return (int) (result - WAIT_ABANDONED_0) * selected_thread;
   } else {
      return (int) (result - WAIT_OBJECT_0) * selected_thread;
   }
}
