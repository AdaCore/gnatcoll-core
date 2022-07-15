------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with GNATCOLL.OS.Win32;
with GNATCOLL.OS.Win32.Files;

separate (GNATCOLL.OS.FS)
procedure Set_Close_On_Exec
  (FD            : File_Descriptor;
   Close_On_Exec : Boolean)
is
   package Win32 renames GNATCOLL.OS.Win32;

   use type Win32.HANDLE;
   Object : Win32.HANDLE;
   Result : Win32.BOOL;
begin
   --  First retrieve the handle associated with the FD
   Object := Win32.Files.GetOSFHandle (FD);

   if Object = Win32.HANDLE'Last then
      --  FD is an invalid handle
      raise OS_Error with "cannot set close on exec on invalid fd" & FD'Img;

   elsif Object = Win32.HANDLE'Last - 1 then
      --  In case the handle is not associated with a stream, any operation on
      --  that handle fails though this is not considered strictly speaking as
      --  an error. See MSDN documentation
      return;
   end if;

   --  Try to set "close on exec"
   Result := Win32.Files.SetHandleInformation
      (Object,
       Win32.Files.HANDLE_FLAG_INHERIT,
       (if Close_On_Exec then 0 else Win32.Files.HANDLE_FLAG_INHERIT));

   if Result = Win32.BOOL_FALSE then
      raise OS_Error with "cannot set close on exec on fd" & FD'Img;
   end if;
end Set_Close_On_Exec;
