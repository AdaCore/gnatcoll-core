------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with System;          use System;
with Interfaces.C;
with Ada.Unchecked_Conversion;

package body GNATCOLL.Plugins is

   type LPCSTR is access constant Interfaces.C.char;
   pragma Convention (C, LPCSTR);

   function LoadLibrary (lpLibFileName : LPCSTR) return Plugin;
   pragma Import (Stdcall, LoadLibrary, "LoadLibraryA");

   procedure FreeLibrary (hModule : Plugin);
   pragma Import (Stdcall, FreeLibrary, "FreeLibrary");

   function GetProcAddress
     (hModule : Plugin; lpProcName : LPCSTR) return Address;
   pragma Import (Stdcall, GetProcAddress, "GetProcAddress");

   function GetLastError return Integer;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   function As_LPCSTR is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => LPCSTR);

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Plugin is
      Local_Path : aliased constant String := Path & ASCII.NUL;
   begin
      return LoadLibrary (As_LPCSTR (Local_Path'Address));
   end Load;

   ---------------------
   -- Routine_Address --
   ---------------------

   function Routine_Address (P : Plugin; Name : String) return Address is
      RN : aliased constant String := Name & ASCII.NUL;
   begin
      return GetProcAddress (P, As_LPCSTR (RN'Address));
   end Routine_Address;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message return String is
   begin
      return "Last error code" & GetLastError'Img;
   end Last_Error_Message;

   ------------
   -- Unload --
   ------------

   procedure Unload (P : in out Plugin) is
   begin
      FreeLibrary (P);
      P := No_Plugin;
   end Unload;

end GNATCOLL.Plugins;
