------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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
procedure Open_Pipe
  (Pipe_Read  : out File_Descriptor;
   Pipe_Write : out File_Descriptor)
is
   package Win32 renames GNATCOLL.OS.Win32;

   Status     : Win32.BOOL;
   Read_Pipe  : Win32.HANDLE;
   Write_Pipe : Win32.HANDLE;
begin

   Status := Win32.Files.CreatePipe
      (Read_Pipe,
       Write_Pipe,
       Win32.No_Inherit_Handle'Access,
       0);

   if Status = Win32.BOOL_FALSE then
      raise OS_Error with "cannot open pipe";
   end if;

   Pipe_Read := Win32.Files.OpenOSFHandle (Read_Pipe);
   Pipe_Write := Win32.Files.OpenOSFHandle (Write_Pipe);

end Open_Pipe;
