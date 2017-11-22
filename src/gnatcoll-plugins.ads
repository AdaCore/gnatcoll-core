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

with System;

package GNATCOLL.Plugins is

   type Plugin is private;
   No_Plugin : constant Plugin;

   function Load (Path : String) return Plugin;
   --  Attempts to load the plugin located at Path.
   --  Returns No_Plugin if plugin not found.

   function Routine_Address
      (P : Plugin; Name : String) return System.Address;
   --  Returns address of the routine named Unit_Name defined in the plugin P.
   --  Returns Null_Address if such routine not found in the plugin.

   function Last_Error_Message return String;
   --  Returns last error message in case of Load or Routine_Address returned
   --  empty result.

   procedure Unload (P : in out Plugin);
   --  Remove the plugin from service. Note the actual effect is
   --  operating-system dependent.

private

   type Plugin is new System.Address;

   No_Plugin : constant Plugin := Plugin (System.Null_Address);

end GNATCOLL.Plugins;
