------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2017-2021, AdaCore                     --
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
with System;
with Ada.Characters.Conversions;

package GNATCOLL.OS is

   --  Exception raised by low OS level function defined under GNATCOLL.OS
   --  hierarchy.
   OS_Error : exception;

   --  Supported OS types
   type OS_Type is (Windows, Unix, MacOS);

   --  The filename resolution policy of a given file system
   type Filename_Casing_Policy is (
      Lower_Case,  --  case insensitive file system, normalized lower case
      Upper_Case,  --  case insensitive file system, normalized upper case
      Preserving,  --  case insensitive file system, case is preserved
      Sensitive    --  case sensitive file system
      );

   --  The following types are used for OS level function calls.

   --  Equivalent to char * in C
   type C_String is new System.Address;

   Null_C_String  : constant C_String;
   Empty_C_String : constant C_String;

   --  Equivalent to wchar * in C
   type C_WString is new System.Address;

   Null_C_WString  : constant C_WString;
   Empty_C_WString : constant C_WString;

   --  Equivalent of char ** in C
   type C_String_Array is new System.Address;
   Null_C_String_Array : constant C_String_Array;

   --  Equivalent of wchar ** in C
   type C_WString_Array is new System.Address;
   Null_C_WString_Array : constant C_WString_Array;

private

   Null_C_String   : constant C_String := C_String (System.Null_Address);
   Empty_String   : constant String := "" & ASCII.NUL;
   Empty_C_String : constant C_String := C_String (Empty_String (1)'Address);

   Null_C_WString  : constant C_WString := C_WString (System.Null_Address);
   Empty_WString  : constant Wide_String :=
      Ada.Characters.Conversions.To_Wide_String ("" & ASCII.NUL);
   Empty_C_WString : constant C_WString :=
      C_WString (Empty_WString (1)'Address);

   Null_C_String_Array : constant C_String_Array :=
      C_String_Array (System.Null_Address);

   Null_C_WString_Array : constant C_WString_Array :=
      C_WString_Array (System.Null_Address);
end GNATCOLL.OS;
