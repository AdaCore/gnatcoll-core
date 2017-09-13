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

--  This package declares various types and subprograms that must be overridden
--  by anyone wishing to add new backends to GNATCOLL.SQL.Exec.
--  Most users can ignore the contents of this package altogether, since none
--  of these types is intended to be visible in the user's code. They are
--  wrapped up in other types in GNATCOLL.SQL.Exec, which is the actual user
--  API.

with GNATCOLL.SQL.Exec;    use GNATCOLL.SQL.Exec;
with Interfaces.C.Strings; use Interfaces.C.Strings;

private package GNATCOLL.SQL.Exec_Private is

   --------------------
   -- Forward_Cursor --
   --------------------

   type DBMS_Forward_Cursor is
      abstract new Abstract_DBMS_Forward_Cursor with null record;
   --  Internal contents of a cursor.
   --  Instead of overriding Cursor directly, the support packages for
   --  the DBMS must override this type, so that Cursor is not visibly
   --  tagged and users do not have to use unconstrained types in their code,
   --  thus allowing "Result : Cursor" declarations.
   --  This type is wrapped by a refcounting record, so that the various
   --  backends do not have to redo it themselves. They can just override
   --  Finalize for the proper finalization of the cursor.

   function Is_Success (Self : DBMS_Forward_Cursor) return Boolean is abstract;
   --  Whether the corresponding query succeeded

   function Has_Row (Self : DBMS_Forward_Cursor) return Boolean is abstract;
   procedure Next   (Self : in out DBMS_Forward_Cursor) is abstract;
   --  See similar subprograms in gnatcoll-sql-exec.ads

   function Error_Msg (Self : DBMS_Forward_Cursor) return String is abstract;
   --  Return the error message associated with the query

   function Status (Self : DBMS_Forward_Cursor) return String is abstract;
   --  Return a string describing the status of the query. This is used for
   --  logging purposes.

   procedure Finalize (Self : in out DBMS_Forward_Cursor) is null;
   --  Free the memory used by Self

   function Processed_Rows
     (Self : DBMS_Forward_Cursor) return Natural is abstract;
   --  Return the number of rows modified by a INSERT, DELETE or UPDATE.
   --  Return the number of rows returned so far by calls to Next for a SELECT.
   --  This isn't the same as Rows_Count, unless we have already iterated over
   --  all results

   function Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return String;
   function C_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Interfaces.C.Strings.chars_ptr is abstract;
   --  Default implementation is to assume the DBMS only returns strings, and
   --  we convert them to the appropriate Ada type.

   function Current (Self : DBMS_Forward_Cursor) return Positive is abstract;
   --  Return the index of the current column (the first one is at index 1)

   function Is_Null
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Boolean is abstract;
   function Last_Id
     (Self       : DBMS_Forward_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer is abstract;
   function Field_Count
     (Self : DBMS_Forward_Cursor) return Field_Index is abstract;
   function Field_Name
     (Self : DBMS_Forward_Cursor; Field : Exec.Field_Index) return String
     is abstract;
   --  See matching subprograms for Query_Result. The default implementation of
   --  the subprograms converts from a string to the appropriate type.
   --  Constraint_Error is raised if the field does not contain an appropriate
   --  value.

   -------------------
   -- Direct_Cursor --
   -------------------

   type DBMS_Direct_Cursor is
      abstract new DBMS_Forward_Cursor with null record;

   procedure First (Self : in out DBMS_Direct_Cursor) is abstract;
   procedure Last  (Self : in out DBMS_Direct_Cursor) is abstract;
   procedure Absolute
     (Self : in out DBMS_Direct_Cursor; Row : Positive) is abstract;
   procedure Relative
     (Self : in out DBMS_Direct_Cursor; Step : Integer) is abstract;
   --  See documentation for GNATCOLL.SQL.Exec.Direct_Cursor

end GNATCOLL.SQL.Exec_Private;
