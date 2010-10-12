-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
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
