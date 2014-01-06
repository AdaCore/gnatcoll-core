------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with Ada.Task_Attributes;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

package body GNATCOLL.SQL.Exec.Tasking is

   Me_Error : constant Trace_Handle := Create ("SQL.ERROR", On);

   package DB_Attributes is new Ada.Task_Attributes
     (Database_Connection, null);

   -------------------------
   -- Get_Task_Connection --
   -------------------------

   function Get_Task_Connection
     (Description : Database_Description;
      Username    : String := "")
      return Database_Connection
   is
      Connection : Database_Connection;
   begin
      Connection := DB_Attributes.Value;
      if Connection = null then
         Connection := Description.Build_Connection;
         if Connection /= null then
            DB_Attributes.Set_Value (Connection);
         else
            Trace
              (Me_Error, "Could not create connection object for database");
         end if;

      else
         Reset_Connection (Connection, Username);
      end if;

      return Connection;
   end Get_Task_Connection;

end GNATCOLL.SQL.Exec.Tasking;
