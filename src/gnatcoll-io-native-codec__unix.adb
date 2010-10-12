-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Unix version: todo, would require interfacing with 'locale'

with Ada.Characters.Handling; use Ada.Characters.Handling;

separate (GNATCOLL.IO.Native)
package body Codec is

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8 (Path : Wide_String) return String is
   begin
      return To_String (Path);
   end To_UTF8;

   function To_UTF8 (Path : FS_String) return String is
   begin
      return String (Path);
   end To_UTF8;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (Path : String) return FS_String is
   begin
      return FS_String (Path);
   end From_UTF8;

   function From_UTF8 (Path : String) return Wide_String is
   begin
      return To_Wide_String (Path);
   end From_UTF8;

end Codec;
