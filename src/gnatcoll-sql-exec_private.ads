------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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
with GNAT.Strings;         use GNAT.Strings;
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
   function Boolean_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Boolean;
   function Integer_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Integer;
   function Float_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Float;
   function Money_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return T_Money;
   function Time_Value
     (Self  : DBMS_Forward_Cursor;
      Field : Field_Index) return Ada.Calendar.Time;
   function Json_Text_Value
     (Self  : DBMS_Forward_Cursor; Field : Field_Index) return String;
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

   generic
      type Forward is new DBMS_Forward_Cursor with private;
   package Generic_Direct_Cursors is
      type Direct is new DBMS_Direct_Cursor with private;
      --  A direct cursor based on Forward. It caches locally the values
      --  returned for each row of Forward, and should be used for systems that
      --  do not have special support for direct cursors.

      type Forward_Access is access all Forward;
      --  Not "access 'Class" to avoid some dynamic dispatching calls, for
      --  efficiency

      procedure Initialize (Self : access Direct; From : access Forward);
      --  Initialize in-memory data for Self, by reading all rows of From.
      --  From is freed by Self when appropriate.

      function Get_Cursor (Self : Direct) return Forward_Access;
      --  Return a pointer to the underlying forward cursor

      overriding function Error_Msg (Self : Direct) return String;
      overriding function Status (Self : Direct) return String;
      overriding function Is_Success (Self : Direct) return Boolean;
      overriding procedure Finalize (Result : in out Direct);
      overriding function Processed_Rows (Self : Direct) return Natural;
      overriding function C_Value
        (Self  : Direct; Field : Field_Index) return chars_ptr;
      overriding function Value
        (Self  : Direct; Field : Field_Index) return String;
      overriding function Is_Null
        (Self  : Direct; Field : Field_Index) return Boolean;
      overriding function Last_Id
        (Self       : Direct;
         Connection : access Database_Connection_Record'Class;
         Field      : SQL_Field_Integer) return Integer;
      overriding function Field_Count (Self : Direct) return Field_Index;
      overriding function Field_Name
        (Self : Direct; Field : Field_Index) return String;
      overriding function Has_Row   (Self : Direct) return Boolean;
      overriding procedure Next     (Self : in out Direct);
      overriding procedure First    (Self : in out Direct);
      overriding procedure Last     (Self : in out Direct);
      overriding function Current (Self : Direct) return Positive;
      overriding procedure Absolute (Self : in out Direct; Row : Positive);
      overriding procedure Relative (Self : in out Direct; Step : Integer);
      overriding function Boolean_Value
        (Self : Direct; Field : Field_Index) return Boolean;

   private
      type Result_Table is
        array (Natural range <>) of GNAT.Strings.String_Access;
      type Result_Table_Access is access all Result_Table;
      --  The results of a SQL query (all the columns of first row, then all
      --  columns of second row,...)

      type Local_Forward is new Forward with record
         Table   : Result_Table_Access := null;
         Columns : Natural := 0;
         --  The cached result. We do not use sqlite3's builtin
         --  sqlite3_get_table since we want to be able to use prepared
         --  statements to query this data.

         Current        : Natural := 0;  --  Current row
      end record;
      --  A local extension, so that we benefit from custom Boolean_Value
      --  defined on Forward, while still dispatching to our Value

      overriding function Value
        (Self  : Local_Forward; Field : Field_Index) return String;
      overriding procedure Finalize (Result : in out Local_Forward);
      overriding function Is_Null
        (Self  : Local_Forward; Field : Field_Index) return Boolean;

      type Local_Forward_Access is access all Local_Forward'Class;

      type Direct is new DBMS_Direct_Cursor with record
         Cursor : Local_Forward_Access;
         --  The cursor that was used to read the results. It has already been
         --  iterated, but provides a handle on the number of rows, the
         --  Statement, and other information.
         --  Also used to get the correct implementation of Boolean_Value,
         --  since that depends on the DBMS.
      end record;
   end Generic_Direct_Cursors;

private

   type DBMS_Connection is
      abstract new Database_Connection_Record with null record;

end GNATCOLL.SQL.Exec_Private;
