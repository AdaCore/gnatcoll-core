------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with GNATCOLL.OS.Win32.Process;
with GNATCOLL.OS.Win32;
with GNATCOLL.OS;

separate (GNATCOLL.OS.Process)
function Wait (H : Process_Handle) return Integer
is
   use GNATCOLL.OS.Win32;
   use GNATCOLL.OS.Win32.Process;

   WHandle   : constant HANDLE := HANDLE (H);
   Exit_Code : DWORD := 1;
   Result    : DWORD;
   Success   : BOOL;

begin
   if H /= 0 then
      Result := Win32.Process.WaitForSingleObject (WHandle, INFINITE);

      if Result /= WAIT_OBJECT_0 then
         Result := GetLastError;
         raise GNATCOLL.OS.OS_Error
         with "error while waiting for process:" & Result'Img;
      end if;

      Success := Win32.Process.GetExitCodeProcess (WHandle, Exit_Code);
      if Success /= BOOL_TRUE then
         Result := GetLastError;
         raise GNATCOLL.OS.OS_Error
         with "error while waiting for process:" & Result'Img;
      end if;

      Success := CloseHandle (WHandle);
      if Success /= BOOL_TRUE then
         Result := GetLastError;
         raise GNATCOLL.OS.OS_Error
         with "error while waiting for process:" & Result'Img;
      end if;
   end if;

   --  On Windows we are sure that Integer is at least 32bits, which makes the
   --  following mapping from DWORD to Integer valid in all cases.
   if Exit_Code > 2 ** 31 - 1 then
      --  Interpret status > 2 ** 31 -1 as negative exit status. Compute 2's
      --  complement to get the expected absolute value. Be carefull that
      --  if Integer size if 32 bits -2 ** 31 is valid but not 2 ** 31.
      Exit_Code := not Exit_Code + 1;
      if Exit_Code = 2 ** 31 then
         return -2 ** 31;
      else
         return -Integer (Exit_Code);
      end if;
   else
      return Integer (Exit_Code);
   end if;
end Wait;
