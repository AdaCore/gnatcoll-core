------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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
------------------------------------------------------------------------------

with GNATCOLL.OS.Win32.Process; use GNATCOLL.OS.Win32.Process;
with GNATCOLL.OS.Win32; use GNATCOLL.OS.Win32;

separate (GNATCOLL.OS.Process)

function Wait_For_Processes
  (Processes : Process_Array;
   Timeout   : Duration := INFINITE_TIMEOUT)
   return Integer
is
   Wait_Result           : Integer;
   Milliseconds_Timeout  : DWORD;
   Processes_To_Wait     : Process_Array (1 .. Processes'Length);
   Processes_Indices     : array (1 .. Processes'Length) of Integer;
   Processes_To_Wait_Num : Integer := 0;
begin
   --  First select the processes that are still in RUNNING state. Note that
   --  reducing the table to the list of processes that can be waited for
   --  might improve significantly the performance.
   for Idx in Processes'Range loop
      declare
         P : constant Process_Handle := Processes (Idx);
         S : constant Process_State := State (P);
      begin
         if S = WAITABLE then
            --  If we have a process in WAITABLE state just return it.
            return Idx;
         elsif S = RUNNING then
            Processes_To_Wait_Num := Processes_To_Wait_Num + 1;
            Processes_To_Wait (Processes_To_Wait_Num) := P;
            Processes_Indices (Processes_To_Wait_Num) := Idx;
         end if;
      end;
   end loop;

   --  If there are no process to wait for just return.
   if Processes_To_Wait_Num = 0 then
      return WAIT_NO_PROCESS;
   end if;

   --  Procedure can wait for 64 threads waiting for 64 processes each
   if Processes_To_Wait_Num > 64 * 64 then
      raise OS_Error with "cannot wait for more than 4096 processes";
   end if;

   --  Compute effective timeout.
   if Timeout >= INFINITE_TIMEOUT then
      --  If Timeout > 4294967 it means we will overflow DWORD'Last after
      --  multiplication by 1000.0. In that case use INFINITE as timeout
      Milliseconds_Timeout := INFINITE;
   elsif Timeout > 0.0 then
      --  Regular timeout
      Milliseconds_Timeout := DWORD (Timeout * 1000.0);
   else
      --  Any non positive value is considered as 0s timeout
      Milliseconds_Timeout := 0;
   end if;

   --  Call GNATCOLL threaded version of WaitForMultipleObjects
   Wait_Result := ThreadedWaitForMultipleObjects
      (Count        => DWORD (Processes_To_Wait_Num),
       Handles      => LPVOID (Processes_To_Wait'Address),
       WaitForAll   => BOOL_FALSE,
       Milliseconds => Milliseconds_Timeout);

   --  Compute final result
   if Wait_Result = -2 then
      return WAIT_TIMEOUT;
   elsif Wait_Result = -1 then
      raise OS_Error with "cannot wait for processes" & GetLastError'Img;
   else
      return Processes_Indices (Processes_To_Wait'First + Wait_Result);
   end if;

end Wait_For_Processes;
