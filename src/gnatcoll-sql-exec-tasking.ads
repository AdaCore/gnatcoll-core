------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

   ----------------------------------------------------------------------------
   --  The database independent cursor implementation. Could be used either  --
   --  to provide Direct_Cursor for databases where it is not supported or   --
   --  to provide data which could be shared between different tasks.        --
   ----------------------------------------------------------------------------

   function Task_Safe_Instance
     (Source   : Forward_Cursor'Class;
      Index_By : Field_Index'Base := No_Field_Index) return Direct_Cursor;
   --  Creates and returns cursor which could be used to clone the copies for
   --  different tasks. This routine creates Source cursor data copy into
   --  internal structures of the resulting cursor. If the Source cursor
   --  already created using this routine, copy is not created but returned the
   --  Source cursor.
   --  Index_By could be supplied to index the result set by some field for
   --  the fast record lookup by the field value. Could be commonly used to
   --  lookup the record by the one field primary key.

   function Task_Safe_Instance
     (Source   : Abstract_Cursor_Access;
      Index_By : Field_Index'Base := No_Field_Index)
      return Abstract_Cursor_Access;
   --  Need to support databases, where direct cursors is not supported.
   --  Returns the same pointer if the Source is already the task safe
   --  direct cursor.

   function Task_Safe_Clone (Source : Direct_Cursor) return Direct_Cursor;
   --  Clone the cursor copy to use in different task.
   --  Source must be the result of a call to Task_Safe_Instance.
   --  The clone have to be made in the task where it will be used.
   --  Each task would use the same data, but own cursor pointer to the current
   --  record. If the Task_Safe_Clone called from the same task where the
   --  Task_Safe_Instance called, the routine returns the same Source cursor to
   --  avoid odd copy.

   procedure Find (Self : Abstract_Cursor_Access; Value : String);
   --  Search the record with specified field value over the internal cursor
   --  index by field defined on Prepare routine call in Index_By parameter.
   --  Set cursor position to the found row. If rows is not indexed, the
   --  Constraint_Error will be raised.

end GNATCOLL.SQL.Exec.Tasking;
