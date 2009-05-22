-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                 Copyright (C) 2005-2009, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package declares various types and subprograms that must be overridden
--  by anyone wishing to add new backends to GNATCOLL.SQL.Exec.
--  Most users can ignore the contents of this package altogether, since none
--  of these types is intended to be visible in the user's code. They are
--  wrapped up in other types in GNATCOLL.SQL.Exec, which is the actual user
--  API.

with GNATCOLL.SQL.Exec;  use GNATCOLL.SQL.Exec;
with System;

private package GNATCOLL.SQL.Exec_Private is

   type DBMS_Cursor is abstract new Abstract_DBMS_Cursor with private;
   --  Internal contents of a cursor.
   --  Instead of overriding Cursor directly, the support packages for
   --  the DBMS must override this type, so that Cursor is not visibly
   --  tagged and users do not have to use unconstrained types in their code,
   --  thus allowing "Result : Cursor" declarations.
   --  This type is wrapped by a refcounting record, so that the various
   --  backends do not have to redo it themselves. They can just override
   --  Finalize for the proper finalization of the cursor.

   function Is_Success (Self : DBMS_Cursor) return Boolean is abstract;
   --  Whether the corresponding query succeeded

   function Has_Row (Self : DBMS_Cursor) return Boolean is abstract;
   procedure Next   (Self : in out DBMS_Cursor) is abstract;
   --  See similar subprograms in gnatcoll-sql-exec.ads

   function Error_Msg (Self : DBMS_Cursor) return String is abstract;
   --  Return the error message associated with the query

   function Status (Self : DBMS_Cursor) return String is abstract;
   --  Return a string describing the status of the query. This is used for
   --  logging purposes.

   procedure Finalize (Self : in out DBMS_Cursor) is abstract;
   --  Free the memory used by Self

   function Rows_Count (Self : DBMS_Cursor) return Natural is abstract;
   --  Return the number of rows impacted (ie modified or returned) by the
   --  query.

   function Processed_Rows (Self : DBMS_Cursor) return Natural is abstract;
   --  Return the number of rows modified by a INSERT, DELETE or UPDATE.
   --  Return the number of rows returned so far by calls to Next for a SELECT.
   --  This isn't the same as Rows_Count, unless we have already iterated over
   --  all results

   function Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return String is abstract;
   function Address_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return System.Address is abstract;
   function Boolean_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Boolean;
   function Integer_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Integer;
   function Float_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Float;
   function Time_Value
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Ada.Calendar.Time;
   --  Default implementation is to assume the DBMS only returns strings, and
   --  we convert them to the appropriate Ada type.

   function Is_Null
     (Self  : DBMS_Cursor;
      Field : Field_Index) return Boolean is abstract;
   function Last_Id
     (Self       : DBMS_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer is abstract;
   function Field_Count (Self : DBMS_Cursor) return Field_Index is abstract;
   function Field_Name
     (Self : DBMS_Cursor; Field : Exec.Field_Index) return String
     is abstract;
   --  See matching subprograms for Query_Result. The default implementation of
   --  the subprograms converts from a string to the appropriate type.
   --  Constraint_Error is raised if the field does not contain an appropriate
   --  value.

private

   type DBMS_Connection is
      abstract new Database_Connection_Record with null record;

   type DBMS_Cursor is abstract
      new GNATCOLL.SQL.Exec.Abstract_DBMS_Cursor with null record;

end GNATCOLL.SQL.Exec_Private;
