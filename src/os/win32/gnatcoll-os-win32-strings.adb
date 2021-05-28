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

package body GNATCOLL.OS.Win32.Strings is

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8
      (Input        : UTF8.UTF_8_String;
       Output       : out Wide_String;
       Output_Start : Integer := From_Start)
      return Integer
   is
      Status : int;
      Offset : Natural := Output'First;
   begin
      if Output_Start >= Output'First then
         Offset := Output_Start;
      end if;

      Status := MultiByteToWideChar (
         UTF8_CodePage,
         DEFAULT_FLAG,
         LPSTR (Input'Address),
         Input'Length,
         LPWSTR (Output (Offset)'Address),
         Output'Length);

      return Integer (Status);
   end From_UTF8;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8
      (Input        : Wide_String;
       Output       : out UTF8.UTF_8_String;
       Output_Start : Integer := From_Start)
      return Integer
   is
      Status : int;
      Offset : Natural := Output'First;
   begin
      if Output_Start >= Output'First then
         Offset := Output_Start;
      end if;

      Status := WideCharToMultiByte
         (UTF8_CodePage, 0,
          LPWSTR (Input'Address),
          Input'Length,
          LPSTR (Output (Offset)'Address),
          Output'Length,
          LPSTR (System.Null_Address),
          null);

      return Integer (Status);
   end To_UTF8;

end GNATCOLL.OS.Win32.Strings;
