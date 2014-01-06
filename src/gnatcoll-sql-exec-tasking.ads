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

--  This package provides additional subprograms to interact with a database
--  in a tasking context.

package GNATCOLL.SQL.Exec.Tasking is

   function Get_Task_Connection
     (Description : Database_Description;
      Username    : String := "")
      return Database_Connection;
   --  Return the database connection specific to the current task. A new one
   --  is created if none existed yet, and the connection to the database is
   --  done automatically.
   --  If the thread is not connected yet, a new connection is created through
   --  Factory.
   --  The newly created connection and Username are then passed to
   --  Reset_Connection (see below).

end GNATCOLL.SQL.Exec.Tasking;
