------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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
    Timeout   : Duration)
   return Process_Handle
is
   Result                : Process_Handle := Invalid_Handle;
   Wait_Result           : DWORD;
   Milliseconds_Timeout  : DWORD;
   Processes_To_Wait     : Process_Array (1 .. Processes'Length);
   Processes_To_Wait_Num : Integer := 0;
begin
   for Idx in Processes'Range loop
      declare
         P : constant Process_Handle := Processes (Idx);
         S : constant Process_State := State (P);
      begin
         if S = WAITABLE then
            return P;
         elsif S = RUNNING then
            Processes_To_Wait_Num := Processes_To_Wait_Num + 1;
            Processes_To_Wait (Processes_To_Wait_Num) := P;
         end if;
      end;
   end loop;

   if Processes_To_Wait_Num = 0 then
      return Invalid_Handle;
   end if;

   if Processes_To_Wait_Num > 64 then
      raise OS_Error with "cannot wait for more than 64 processes";
   end if;

   if Timeout < 0.0 then
      Milliseconds_Timeout := INFINITE;
   else
      Milliseconds_Timeout := DWORD (Timeout * 1000.0);
   end if;

   Wait_Result := WaitForMultipleObjects
      (Count        => DWORD (Processes_To_Wait_Num),
       Handles      => LPVOID (Processes_To_Wait'Address),
       WaitForAll   => BOOL_FALSE,
       Milliseconds => Milliseconds_Timeout);

   if Wait_Result = WAIT_TIMEOUT then
      return Invalid_Handle;
   elsif Wait_Result = WAIT_FAILED then
      raise OS_Error with "cannot wait for processes" & GetLastError'Img;
   elsif Wait_Result >= WAIT_ABANDONED_0 and then
      Wait_Result < WAIT_ABANDONED_0 + DWORD (Processes_To_Wait_Num)
   then
      Result := Processes_To_Wait
         (Processes_To_Wait'First + Integer (Wait_Result - WAIT_ABANDONED_0));
   elsif Wait_Result >= WAIT_OBJECT_0 and then
      Wait_Result < WAIT_OBJECT_0 + DWORD (Processes_To_Wait_Num)
   then
      Result :=
         Processes_To_Wait (Processes_To_Wait'First + Integer (Wait_Result));
   end if;
   return Result;
end Wait_For_Processes;
