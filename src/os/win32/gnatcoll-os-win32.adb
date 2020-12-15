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

with GNATCOLL.OS.Win32.Strings; use GNATCOLL.OS.Win32.Strings;

package body GNATCOLL.OS.Win32 is

   procedure Initialize (Path : in out UNICODE_PATH; Str : String := "") is
   begin
      Path.Str.Buffer := LPWSTR (Path.Buffer'Address);
      Path.Str.Length := 0;
      Path.Str.MaximumLength := USHORT (32767 * Wide_Character'Size / 8);
      Append (Path, "\??\");
      Append (Path, Str);
   end Initialize;

   procedure Append (Path : in out UNICODE_PATH; Str : String) is
      WPath_Size : Integer := 0;
   begin
      WPath_Size := From_UTF8 (
         Str, Path.Buffer,
         Integer (Path.Str.Length) / 2 + Path.Buffer'First);
      Path.Str.Length := Path.Str.Length +
         USHORT (WPath_Size * Wide_Character'Size / 8);
   end Append;

   function Is_Success (Status : NTSTATUS) return Boolean
   is
   begin
      return Status >= 0;
   end Is_Success;
end GNATCOLL.OS.Win32;
