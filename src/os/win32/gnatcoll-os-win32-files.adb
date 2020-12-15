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

package body GNATCOLL.OS.Win32.Files is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (FA   : in out FILE_OBJECT_ATTRIBUTES;
                         Name : String)
   is
   begin
      Initialize (FA.Path, Name);
      FA.OA.Length := OBJECT_ATTRIBUTES'Size / 8;
      FA.OA.RootDirectory := NULL_HANDLE;
      FA.OA.ObjectName    := FA.Path.Str'Unrestricted_Access;
      FA.OA.Attributes     := 16#40#;
      FA.OA.SecurityDescriptor := NULL_LPVOID;
      FA.OA.SecurityQualityOfService := NULL_LPVOID;
   end Initialize;

end GNATCOLL.OS.Win32.Files;
