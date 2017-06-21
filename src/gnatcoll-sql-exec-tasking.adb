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

pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;
with Ada.Task_Attributes;
with Ada.Task_Identification;    use Ada.Task_Identification;
with GNATCOLL.Strings;           use GNATCOLL.Strings;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Refcount;          use GNATCOLL.Refcount;
with GNATCOLL.SQL.Exec_Private;  use GNATCOLL.SQL.Exec_Private;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body GNATCOLL.SQL.Exec.Tasking is

   use type Ada.Containers.Count_Type;

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

   ---------------------------------------
   --  Task safe cursors implementation --
   ---------------------------------------

   type Field_Value is record
      Element : XString;
      Empty   : Boolean := False;
   end record;

   type Record_Type is array (Field_Index range <>) of Field_Value;

   package Data_Set_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, Record_Type);
   --  Zero index is for field names

   package String_Indexes is new Ada.Containers.Hashed_Maps
     (XString, Positive, Hash, Equivalent_Keys => "=");

   type Data_Set is record
      Table   : Data_Set_Vectors.Vector;
      Index   : String_Indexes.Map;
      Error   : XString;
      Status  : XString;
      TID     : Task_Id; -- Keep the Task ID where the cursor is filled
      Success : Boolean;
   end record;

   package Data_Set_Pointers is new Shared_Pointers (Data_Set);

   type Task_Cursor is new DBMS_Direct_Cursor with record
      Position : Natural := 1;
      Data     : Data_Set_Pointers.Ref;
   end record;

   overriding function Is_Success (Self : Task_Cursor) return Boolean
   is (Self.Data.Get.Success);

   overriding function Is_Null
     (Self : Task_Cursor; Field : Field_Index) return Boolean
   is (Self.Data.Get.Table (Self.Position) (Field).Empty);

   overriding function Error_Msg (Self : Task_Cursor) return String
   is (To_String (Self.Data.Get.Error));

   overriding function Status (Self : Task_Cursor) return String
   is (To_String (Self.Data.Get.Status));

   overriding function Has_Row (Self : Task_Cursor) return Boolean
   is (Self.Position <= Self.Data.Get.Table.Last_Index);

   overriding function Processed_Rows (Self : Task_Cursor) return Natural
   is (Self.Data.Get.Table.Last_Index);

   overriding function Current (Self : Task_Cursor) return Positive
   is (Self.Position);

   overriding function Field_Name
     (Self : Task_Cursor; Field : Field_Index) return String
   is (To_String (Self.Data.Get.Table (0) (Field).Element));

   overriding function Field_Count (Self : Task_Cursor) return Field_Index
   is (Self.Data.Get.Table (0).Element'Length);

   overriding procedure Next (Self : in out Task_Cursor);

   overriding function C_Value
     (Self  : Task_Cursor; Field : Field_Index) return chars_ptr;

   overriding function Last_Id
     (Self       : Task_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer;

   overriding procedure First (Self : in out Task_Cursor);
   overriding procedure Last (Self : in out Task_Cursor);
   overriding procedure Absolute (Self : in out Task_Cursor; Row : Positive);
   overriding procedure Relative (Self : in out Task_Cursor; Step : Integer);

   overriding function Value
     (Self : Task_Cursor; Field : Field_Index) return String;

   type Task_Cursor_Access is access all Task_Cursor;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Task_Cursor) is
   begin
      Self.Position := Self.Position + 1;
   end Next;

   -------------
   -- C_Value --
   -------------

   overriding function C_Value
     (Self  : Task_Cursor; Field : Field_Index) return chars_ptr
   is
      pragma Unreferenced (Self, Field);
   begin
      raise Program_Error with
        "Should not be used, because all data in Ada strings already";
      return Null_Ptr;
   end C_Value;

   -------------
   -- Last_Id --
   -------------

   overriding function Last_Id
     (Self       : Task_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer
   is
      pragma Unreferenced (Self, Field);
   begin
      raise Program_Error with
        "Task safe cursors is not supported for insert statements";
      return 0;
   end Last_Id;

   -----------
   -- First --
   -----------

   overriding procedure First (Self : in out Task_Cursor) is
   begin
      Self.Position := 1;
   end First;

   ----------
   -- Last --
   ----------

   overriding procedure Last (Self : in out Task_Cursor) is
   begin
      Self.Position := Self.Data.Get.Table.Last_Index;
   end Last;

   --------------
   -- Absolute --
   --------------

   overriding procedure Absolute (Self : in out Task_Cursor; Row : Positive) is
   begin
      Self.Position := Row;
   end Absolute;

   --------------
   -- Relative --
   --------------

   overriding procedure Relative (Self : in out Task_Cursor; Step : Integer) is
   begin
      Self.Position := Self.Position + Step;
   end Relative;

   ----------
   -- Find --
   ----------

   procedure Find (Self : Abstract_Cursor_Access; Value : String) is
      TC : Task_Cursor_Access;
      C : String_Indexes.Cursor;
      Not_Indexed : constant String := "Cursor is not indexed.";
   begin
      if Self.all not in Task_Cursor'Class then
         raise Constraint_Error with Not_Indexed;
      end if;

      TC := Task_Cursor_Access (Self);

      if TC.Data.Get.Index.Length = 0
        and then TC.Data.Get.Table.Length > 0
      then
         raise Constraint_Error with Not_Indexed;
      end if;

      C := TC.Data.Get.Index.Find (To_XString (Value));

      if String_Indexes.Has_Element (C) then
         TC.Position := String_Indexes.Element (C);
      else
         TC.Position := TC.Data.Get.Table.Last_Index + 1;
      end if;
   end Find;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self : Task_Cursor; Field : Field_Index) return String is
   begin
      return To_String (Self.Data.Get.Table (Self.Position) (Field).Element);
   end Value;

   ------------------------
   -- Task_Safe_Instance --
   ------------------------

   function Task_Safe_Instance
     (Source   : Abstract_Cursor_Access;
      Index_By : Field_Index'Base := No_Field_Index)
      return Abstract_Cursor_Access
   is
      Src : DBMS_Forward_Cursor'Class renames
        DBMS_Forward_Cursor'Class (Source.all);

      Result : Task_Cursor_Access;
      Row : Record_Type (Field_Index'First
                         .. Src.Field_Count + Field_Index'First - 1);

      procedure Append_Index_Element (Row : Positive);

      --------------------------
      -- Append_Index_Element --
      --------------------------

      procedure Append_Index_Element (Row : Positive) is
         CS : String_Indexes.Cursor;
         OK : Boolean;
         Ref : constant Data_Set_Pointers.Reference_Type := Result.Data.Get;
      begin
         Ref.Index.Insert (Ref.Table (Row)(Index_By).Element, Row, CS, OK);

         if not OK then
            Trace
              (Me_Error,
               "Field " & Src.Field_Name (Index_By) & " value "
               & To_String (Ref.Table (Row)(Index_By).Element)
               & " is not unique. Not all records indexed.");
            --  We could support a few records response on one Find call
            --  over the Next after Find.
         end if;
      end Append_Index_Element;

   begin
      if Source.all in Task_Cursor'Class then
         Result := Task_Cursor_Access (Source);

         if Index_By >= Field_Index'First
           and then Result.Data.Get.Index.Is_Empty
         then
            for J in 1 .. Result.Data.Get.Table.Last_Index loop
               Append_Index_Element (J);
            end loop;
         end if;

         return Source;
      end if;

      Result := new Task_Cursor;

      Result.Data.Set (Data_Set'
                         (Error   => To_XString (Src.Error_Msg),
                          Status  => To_XString (Src.Status),
                          Success => Src.Is_Success,
                          TID     => Current_Task,
                          Table   => <>,
                          Index   => <>));

      for J in Row'Range loop
         Row (J).Element := To_XString (Src.Field_Name (J));
      end loop;

      Result.Data.Get.Table.Append (Row);

      while Src.Has_Row loop
         for J in Row'Range loop
            if Src.Is_Null (J) then
               Row (J) := (Null_XString, True);
            else
               Row (J).Element := To_XString (Src.Value (J));
               Row (J).Empty := False;
            end if;
         end loop;

         Result.Data.Get.Table.Append (Row);

         if Index_By >= Field_Index'First then
            Append_Index_Element (Result.Data.Get.Table.Last_Index);
         end if;

         Src.Next;
      end loop;

      if Src in DBMS_Direct_Cursor'Class then
         First (DBMS_Direct_Cursor'Class (Src));
      end if;

      return Abstract_Cursor_Access (Result);
   end Task_Safe_Instance;

   function Task_Safe_Instance
     (Source   : Forward_Cursor'Class;
      Index_By : Field_Index'Base := No_Field_Index) return Direct_Cursor
   is
      Target : constant Abstract_Cursor_Access :=
        Task_Safe_Instance (Source.Res, Index_By);
   begin
      if Target = Source.Res then
         return Direct_Cursor (Source);
      end if;

      return (Ada.Finalization.Controlled with
              Res => Target, Format => Source.Format);
   end Task_Safe_Instance;

   ---------------------
   -- Task_Safe_Clone --
   ---------------------

   function Task_Safe_Clone (Source : Direct_Cursor) return Direct_Cursor is
      Data : constant Data_Set_Pointers.Ref :=
        Task_Cursor_Access (Source.Res).Data;
   begin
      if Data.Get.TID = Current_Task then
         --  Do not need to create a copy for the task where the cursor created

         return Source;
      end if;

      return (Ada.Finalization.Controlled with
              Format => Source.Format,
              Res => new Task_Cursor'
                           (Abstract_DBMS_Forward_Cursor with
                            Position => 1, Data => Data));
   end Task_Safe_Clone;

end GNATCOLL.SQL.Exec.Tasking;
