------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

--  Declare some basic types used in Win32 API calls
--
--  Documentation for these types can be found on Microsoft MSDN

with Interfaces.C; use Interfaces.C;
with System;
pragma Warnings (Off);
with System.Parameters;
pragma Warnings (On);

package GNATCOLL.OS.Win32 is

   subtype CHAR is Character;
   subtype WCHAR is wchar_t;

   type LPWSTR is new System.Address;
   type LPSTR  is new System.Address;

   subtype USHORT is unsigned_short;
   type LPVOID is new System.Address;
   subtype BOOL is Integer;

   BOOL_FALSE : constant BOOL := 0;
   BOOL_TRUE  : constant BOOL := 1;

   type LONG is range -(2 ** (System.Parameters.long_bits - Integer'(1)))
     .. +(2 ** (System.Parameters.long_bits - Integer'(1))) - 1;

   type LPBOOL is access all BOOL;
   type WORD is mod 2 ** Short_Integer'Size;
   type DWORD is mod 2 ** LONG'Size;
   type ULONG is mod 2 ** LONG'Size;
   type ULONG_PTR is mod 2 ** Standard'Address_Size;
   type UINT is mod 2 ** Integer'Size;
   subtype NTSTATUS is LONG;
   subtype LONGLONG is Long_Long_Integer;
   type ULONGLONG is mod 2 ** 64;

   subtype LARGE_INTEGER is LONGLONG;
   type PLARGE_INTEGER is access all LARGE_INTEGER;

   type HANDLE is mod 2 ** Standard'Address_Size;
   type PHANDLE is access all HANDLE;
   NULL_LPVOID : constant LPVOID := LPVOID (System.Null_Address);
   NULL_HANDLE : constant HANDLE := HANDLE (0);
   INVALID_HANDLE_VALUE : constant HANDLE := HANDLE'Last;

   --  Unicode strings used by Windows Native interfaces
   type UNICODE_STRING is record
      Length        : USHORT;
      MaximumLength : USHORT;
      Buffer        : LPWSTR;
   end record
   with Convention => C_Pass_By_Copy;

   type UNICODE_PATH is record
      Str    : UNICODE_STRING;
      Buffer : Wide_String (1 .. 32767);
   end record;

   type PUNICODE_STRING is access all UNICODE_STRING;

   procedure Initialize (Path : in out UNICODE_PATH; Str : String := "");
   --  Initialize a UNICODE_PATH

   procedure Append (Path : in out UNICODE_PATH; Str : String);
   --  Append Str to Path

   function Is_Success (Status : NTSTATUS) return Boolean
      with Inline;
   --  Return True if Status is a successful status

   type SECURITY_ATTRIBUTES is record
      Length             : DWORD;
      SecurityDescriptor : LPVOID;
      InheritHandle      : BOOL;
   end record;
   type LPSECURITY_ATTRIBUTES is access all SECURITY_ATTRIBUTES;

   Inherit_Handle : aliased SECURITY_ATTRIBUTES :=
      (Standard'Address_Size + 2 * Integer'Size,
       LPVOID (System.Null_Address),
       BOOL_TRUE);

   No_Inherit_Handle : aliased SECURITY_ATTRIBUTES :=
      (Standard'Address_Size + 2 * Integer'Size,
       LPVOID (System.Null_Address),
       BOOL_FALSE);

   function GetLastError return DWORD
   with Import        => True,
        Convention    => Stdcall,
        External_Name => "GetLastError";

   Win32_Epoch_Offset : constant LARGE_INTEGER := 11644473600;
   --  Difference between Win32 epoch offset and linux epoch in seconds

end GNATCOLL.OS.Win32;
