------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;
with Ada.Task_Attributes;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Task_Identification;   use Ada.Task_Identification;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Refcount;         use GNATCOLL.Refcount;
with GNATCOLL.SQL.Exec_Private; use GNATCOLL.SQL.Exec_Private;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

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

   package Fields is new Ada.Containers.Indefinite_Holders (String);
   type Record_Type is array (Field_Index range <>) of Fields.Holder;

   package Data_Set_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, Record_Type);
   --  Zero index is for field names

   type Data_Set is record
      Table   : Data_Set_Vectors.Vector;
      Error   : Unbounded_String;
      Status  : Unbounded_String;
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
   is (Self.Data.Get.Table (Self.Position) (Field).Is_Empty);

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
   is (Self.Data.Get.Table (0) (Field).Element);

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

   overriding function Boolean_Value
     (Self : Task_Cursor; Field : Field_Index) return Boolean;

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

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : Task_Cursor; Field : Field_Index) return Boolean
   is
      Value : constant String := Self.Value (Field);
   begin
      return Value'Length > 0
             and then (Value (Value'First) = 't'
                       or else Value (Value'First) = 'T'
                       or else Value (Value'First) in '1' .. '9');
   end Boolean_Value;

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

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self : Task_Cursor; Field : Field_Index) return String
   is
      Holder : constant Fields.Holder :=
         Self.Data.Get.Table (Self.Position) (Field);
   begin
      if Holder.Is_Empty then
         return "";
      else
         return Holder.Element;
      end if;
   end Value;

   ------------------------
   -- Task_Safe_Instance --
   ------------------------

   function Task_Safe_Instance
     (Source : Abstract_Cursor_Access) return Abstract_Cursor_Access
   is
      Src : DBMS_Forward_Cursor'Class renames
        DBMS_Forward_Cursor'Class (Source.all);

      Result : Task_Cursor_Access;
      Row : Record_Type (Field_Index'First
                         .. Src.Field_Count + Field_Index'First - 1);
   begin
      if Source.all in Task_Cursor'Class then
         return Source;
      end if;

      Result := new Task_Cursor;

      Result.Data.Set (Data_Set'
                         (Error   => To_Unbounded_String (Src.Error_Msg),
                          Status  => To_Unbounded_String (Src.Status),
                          Success => Src.Is_Success,
                          TID     => Current_Task,
                          Table   => <>));

      for J in Row'Range loop
         Row (J).Replace_Element (Src.Field_Name (J));
      end loop;

      Result.Data.Get.Table.Append (Row);

      while Src.Has_Row loop
         for J in Row'Range loop
            if Src.Is_Null (J) then
               if not Row (J).Is_Empty then
                  --  GNAT bug workaround on clear of empty holder,
                  --  Fixed in O216-003.
                  Row (J).Clear;
               end if;
            else
               Row (J).Replace_Element (Src.Value (J));
            end if;
         end loop;

         Result.Data.Get.Table.Append (Row);

         Src.Next;
      end loop;

      if Src in DBMS_Direct_Cursor'Class then
         First (DBMS_Direct_Cursor'Class (Src));
      end if;

      return Abstract_Cursor_Access (Result);
   end Task_Safe_Instance;

   function Task_Safe_Instance
     (Source : Forward_Cursor'Class) return Direct_Cursor
   is
      Target : constant Abstract_Cursor_Access :=
         Task_Safe_Instance (Source.Res);
   begin
      if Target = Source.Res then
         return Direct_Cursor (Source);
      end if;

      return (Ada.Finalization.Controlled with Res => Target);
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
              Res => new Task_Cursor'
                           (Abstract_DBMS_Forward_Cursor with
                            Position => 1, Data => Data));
   end Task_Safe_Clone;

end GNATCOLL.SQL.Exec.Tasking;
