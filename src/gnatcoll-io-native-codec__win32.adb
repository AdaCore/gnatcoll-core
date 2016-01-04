------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

--  Windows version: all paths are already utf-8 encoded by mingw when
--  GNAT_CODE_PAGE is CP_UTF8. Otherwise we need to convert to/from UTF-8.

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with GNAT.Decode_UTF8_String;   use GNAT.Decode_UTF8_String;
with GNAT.Encode_UTF8_String;   use GNAT.Encode_UTF8_String;

separate (GNATCOLL.IO.Native)
package body Codec is

   Is_UTF8 : Boolean := True;

   procedure Initialize;
   --  Initialize current Windows code page

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Exists ("GNAT_CODE_PAGE")
        and then Value ("GNAT_CODE_PAGE") = "CP_ACP"
      then
         Is_UTF8 := False;
      end if;
   end Initialize;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8 (Path : Wide_String) return String is
   begin
      if Is_UTF8 then
         return To_String (Path);
      else
         return Encode_Wide_String (Path);
      end if;
   end To_UTF8;

   function To_UTF8 (Path : FS_String) return String is
   begin
      if Is_UTF8 then
         return String (Path);
      else
         return Encode_Wide_String (To_Wide_String (String (Path)));
      end if;
   end To_UTF8;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (Path : String) return Wide_String is
   begin
      if Is_UTF8 then
         return Decode_Wide_String (Path);
      else
         return To_Wide_String (Path);
      end if;
   end From_UTF8;

   function From_UTF8 (Path : String) return FS_String is
   begin
      return FS_String (To_String (From_UTF8 (Path)));
   end From_UTF8;

begin
   Initialize;
end Codec;
