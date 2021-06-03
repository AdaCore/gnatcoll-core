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

package body GNATCOLL.OS.Dir is

   ----------------
   -- Attributes --
   ----------------

   function Attributes (Self : Dir_Entry) return Stat.File_Attributes
   is
   begin
      if End_Of_Iteration (Self) then
         raise OS_Error with "invalid directory entry";
      end if;
      return Self.Info;
   end Attributes;

   -----------
   -- Close --
   -----------

   procedure Close (Handle : Dir_Handle) is separate;

   ----------------------
   -- End_Of_Iteration --
   ----------------------

   function End_Of_Iteration (Self : Dir_Entry) return Boolean is
   begin
      return Self.Name_Last = 0;
   end End_Of_Iteration;

   ----------
   -- Name --
   ----------

   function Name (Self : Dir_Entry) return UTF8.UTF_8_String is
   begin
      if End_Of_Iteration (Self) then
         raise OS_Error with "invalid directory entry";
      end if;

      return Self.Name_Buffer (1 .. Self.Name_Last);
   end Name;

   ----------
   -- Open --
   ----------

   function Open (Path : UTF8.UTF_8_String) return Dir_Handle is separate;

   ----------
   -- Read --
   ----------

   function Read (Handle : Dir_Handle) return Dir_Entry is separate;

end GNATCOLL.OS.Dir;
