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

--  Win32 strings conversion functions.

with Ada.Strings.UTF_Encoding;

package GNATCOLL.OS.Win32.Strings is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   subtype CodePage is UINT;
   subtype MB2WC_Flag is DWORD;
   subtype WC2MB_Flag is DWORD;

   --  MultiByteToWideChar flags
   DEFAULT_FLAG      : constant MB2WC_Flag := 0;
   PRECOMPOSED       : constant MB2WC_Flag := 1;
   COMPOSITE         : constant MB2WC_Flag := 2;
   USEGLYPHCHARS     : constant MB2WC_Flag := 4;
   ERR_INVALID_CHARS : constant MB2WC_Flag := 8;

   --  Codepages
   UTF8_CodePage : constant CodePage := 65001;

   --  Special constant used by From_UTF8 and To_UTF8
   From_Start : constant Integer := Integer'First;

   function From_UTF8
      (Input        : UTF8.UTF_8_String;
       Output       : out Wide_String;
       Output_Start : Integer := From_Start)
      return Integer;
   --  Convert an UTF-8 string into a Wide_String and copy the result in Output
   --  at position Output_Start. If Output_Start is equal to From_Start then
   --  starts at Output'First.

   function To_UTF8
      (Input        : Wide_String;
       Output       : out UTF8.UTF_8_String;
       Output_Start : Integer := From_Start)
      return Integer;
   --  Convert a Wide_String into an UTF-8 string and copy the result in Output
   --  at position Output_Start. If Output_Start is equal to From_Start then
   --  starts at Output'First.

   function MultiByteToWideChar
     (Page         : CodePage;
      Flags        : MB2WC_Flag;
      MultiByteStr : LPSTR;
      MultiByte    : int;
      WideCharStr  : LPWSTR;
      WideChar     : int) return int
   with Import => True,
        Convention => Stdcall,
        External_Name => "MultiByteToWideChar";

   function WideCharToMultiByte
     (Page            : CodePage;
      Flags           : WC2MB_Flag;
      WideCharStr     : LPWSTR;
      WideChar        : int;
      MultiByteStr    : LPSTR;
      MultiByte       : int;
      DefaultChar     : LPSTR;
      UsedDefaultChar : LPBOOL)
      return int
   with Import => True,
        Convention => Stdcall,
        External_Name => "WideCharToMultiByte";

end GNATCOLL.OS.Win32.Strings;
