------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

package body GNATCOLL.SQL.Sqlite.Builder is

   ----------------------
   -- Build_Connection --
   ----------------------

   function Build_Connection
      (Descr : access Sqlite_Description'Class) return Database_Connection
   is
      pragma Unreferenced (Descr);
   begin
      return null;
   end Build_Connection;

   ------------------------
   -- Has_Sqlite_Support --
   ------------------------

   function Has_Sqlite_Support return Boolean is
   begin
      return False;
   end Has_Sqlite_Support;

   -----------
   -- Setup --
   -----------

   procedure Setup is
   begin
      null;
   end Setup;

end GNATCOLL.SQL.Sqlite.Builder;
