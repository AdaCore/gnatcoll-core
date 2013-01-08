------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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
