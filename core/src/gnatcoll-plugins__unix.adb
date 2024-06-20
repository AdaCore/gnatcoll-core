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

with System;               use System;
with Interfaces.C.Strings; use Interfaces;

package body GNATCOLL.Plugins is

   ----------
   -- Load --
   ----------

   function Load (Path : String) return Plugin is
      function dlopen (Lib_Name : String; Mode : C.int) return Plugin;
      pragma Import (C, dlopen, "dlopen");

      RTLD_LAZY : constant := 1;
      C_Path    : constant String := Path & ASCII.NUL;
   begin
      return dlopen (C_Path, RTLD_LAZY);
   end Load;

   ---------------------
   -- Routine_Address --
   ---------------------

   function Routine_Address (P : Plugin; Name : String) return Address is
      function dlsym (Handle : Plugin; Sym_Name : String) return Address;
      pragma Import (C, dlsym, "dlsym");

      C_Name : constant String := Name & ASCII.NUL;
   begin
      return dlsym (Handle => P, Sym_Name => C_Name);
   end Routine_Address;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message return String is
      function dlerror return C.Strings.chars_ptr;
      pragma Import (C, dlerror, "dlerror");
   begin
      return C.Strings.Value (dlerror);
   end Last_Error_Message;

   ------------
   -- Unload --
   ------------

   procedure Unload (P : in out Plugin) is
      function dlclose (Handle : Plugin) return C.int;
      pragma Import (C, dlclose, "dlclose");

      Ignored : C.int;
      pragma Unreferenced (Ignored);
   begin
      Ignored := dlclose (P);
   end Unload;

end GNATCOLL.Plugins;
