------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with GNATCOLL.ALI.Database; use GNATCOLL.ALI.Database;
with GNATCOLL.Mmap;         use GNATCOLL.Mmap;
with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

package body GNATCOLL.ALI is
   use Library_Info_Lists;

   Me_Error   : constant Trace_Handle := Create ("ENTITIES.ERROR");
   Me_Debug   : constant Trace_Handle := Create ("ENTITIES.DEBUG", Off);
   Me_Forward : constant Trace_Handle := Create ("ENTITIES.FORWARD");
   Me_Timing  : constant Trace_Handle := Create ("ENTITIES.TIMING");

   Instances_Provide_Column : constant Boolean := False;
   --  Whether instance info in the ALI files provide the column information.
   --  This is not the case currently, but this requires additional queries
   --  that could be avoided otherwise.

   ALI_Contains_External_Refs : constant Boolean := True;
   --  Given U is the set of units for a given ALI file (corresponding to the
   --  U lines).
   --  This variable should be set to True if an ALI file can contain
   --  references to entities defined in a file not in U, when the reference is
   --  also not in a file from U.
   --  The parser does extra tests in this case to remove duplicate references
   --  that would occur in the database otherwise.
   --  This must be left to True when parsing .gli files since these do have
   --  duplicates. However, this constant was left as a documentation of the
   --  impact this has on the parsing of ALI files.

   Memory_Threshold : constant Ada.Containers.Count_Type := 150;
   --  Number of LI files to update after which we will use a temporary
   --  in-memory copy of the database. This is computed as such:
   --    on the GPS project, it takes about .0230s per LI file when db is in
   --    memory; but we need 5.822s to recreate the indexes and about 6s to
   --    dump the database from memory to the disk.
   --    When working directly on the disk, it takes about .082s per file,
   --    with no additional cost.
   --  This gives a threshold of about 190 for my machine.

   type Access_String is access constant String;
   function Convert is new Ada.Unchecked_Conversion
     (Cst_Filesystem_String_Access, Access_String);

   Query_Get_File : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Files.Id & Database.Files.Stamp,
             From => Database.Files,
             Where => Database.Files.Path = Text_Param (1),
             Limit => 1),
        On_Server => True, Name => "get_file");
   --  Retrieve the info for a file given its path

   Query_Update_LI_File : constant Prepared_Statement :=
     Prepare
       (SQL_Update
            (Set   => Database.Files.Stamp = Time_Param (2),
             Table => Database.Files,
             Where => Database.Files.Id = Integer_Param (1)),
        On_Server => True, Name => "update_li_file");

   Query_Insert_LI_File : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Files.Path = Text_Param (1))
             & (Database.Files.Stamp = Time_Param (2))
             & (Database.Files.Language = "li")),
        On_Server => True, Name => "insert_li_file");

   Query_Insert_Source_File : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Files.Path = Text_Param (1))
             & (Database.Files.Language = Text_Param (2))),
        On_Server => True, Name => "insert_source_file");

   Query_Set_File_Dep : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.F2f.Fromfile = Integer_Param (1))
             & (Database.F2f.Tofile = Integer_Param (2))
             & (Database.F2f.Kind = F2f_Withs)),
        On_Server => True, Name => "set_file_dep");

   Query_Set_ALI : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.F2f.Fromfile = Integer_Param (1))
             & (Database.F2f.Tofile = Integer_Param (2))
             & (Database.F2f.Kind = F2f_Has_Ali)),
        On_Server => True, Name => "set_ali");

   Query_Delete_File_Dep : constant Prepared_Statement :=
     Prepare
       (SQL_Delete
            (From => Database.F2f,
             Where => Database.F2f.Fromfile = Integer_Param (1)),
        On_Server => True, Name => "delete_file_dep");
   --  Delete the f2f relationships for the given file.

   Query_Delete_E2E_From_LI : constant Prepared_Statement :=
     Prepare
       (SQL_Delete
            (From  => Database.E2e,
             Where => Database.E2e.From_LI = Integer_Param (1)),
        On_Server => True, Name => "delete_e2e");

   Query_Delete_Refs : constant Prepared_Statement :=
     Prepare
       (SQL_Delete
            (From  => Database.Entity_Refs,
             Where => Database.Entity_Refs.File = Integer_Param (1)),
        On_Server => True, Name => "delete_refs");

   Query_Insert_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entities.Name = Text_Param (1))
             & (Database.Entities.Kind = Text_Param (2))
             & (Database.Entities.Decl_File = Integer_Param (3))
             & (Database.Entities.Decl_Line = Integer_Param (4))
             & (Database.Entities.Decl_Column = Integer_Param (5))),
        On_Server => True, Name => "insert_entity");

   Query_Insert_Ref : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entity_Refs.Entity   = Integer_Param (1))
             & (Database.Entity_Refs.File   = Integer_Param (2))
             & (Database.Entity_Refs.Line   = Integer_Param (3))
             & (Database.Entity_Refs.Column = Integer_Param (4))
             & (Database.Entity_Refs.Kind   = Text_Param (5))
             & (Database.Entity_Refs.From_Instantiation = Text_Param (6))),
        On_Server => True, Name => "insert_ref");

   Query_Insert_Ref_With_Caller : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entity_Refs.Entity   = Integer_Param (1))
             & (Database.Entity_Refs.File   = Integer_Param (2))
             & (Database.Entity_Refs.Line   = Integer_Param (3))
             & (Database.Entity_Refs.Column = Integer_Param (4))
             & (Database.Entity_Refs.Kind   = Text_Param (5))
             & (Database.Entity_Refs.From_Instantiation = Text_Param (6))
             & (Database.Entity_Refs.Caller = Integer_Param (7))),
        On_Server => True, Name => "insert_ref_with_caller");

   Query_Insert_E2E : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.E2e.Fromentity = Integer_Param (1))
             & (Database.E2e.Toentity = Integer_Param (2))
             & (Database.E2e.Kind = Integer_Param (3))
             & (Database.E2e.Order_By = Integer_Param (4))
             & (Database.E2e.From_LI = Integer_Param (5))),
        On_Server => True, Name => "insert_e2e");

   Query_Find_Entity_From_Decl : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Entities.Id & Database.Entities.Name,
             From => Database.Entities,
             Where => Database.Entities.Decl_File = Integer_Param (1)
             and Database.Entities.Decl_Line = Integer_Param (2)
             and Database.Entities.Decl_Column = Integer_Param (3)),
        On_Server => True, Name => "entity_from_decl");
   Query_Find_Entity_From_Decl_No_Column : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Entities.Id & Database.Entities.Name,
             From => Database.Entities,
             Where => Database.Entities.Decl_File = Integer_Param (1)
             and Database.Entities.Decl_Line = Integer_Param (2)),
        On_Server => True, Name => "entity_from_decl_no_column");
   Query_Find_Predefined_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Entities.Id & Database.Entities.Name,
             From => Database.Entities,
             Where => Database.Entities.Decl_File = -1
             and Database.Entities.Decl_Line = -1
             and Database.Entities.Decl_Column = -1
             and Database.Entities.Name = Text_Param (1),
             Limit => 1),
        On_Server => True, Name => "predefined_entity");
   --  Get an entity's id given the location of its declaration. In sqlite3,
   --  this is implemented as a single table lookup thanks to the multi-column
   --  covering index we created.

   Query_Set_Entity_Renames : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            (Values => (Database.E2e.Fromentity = Integer_Param (1))
             & (Database.E2e.Toentity = Database.Entity_Refs.Entity)
             & (Database.E2e.Kind = Integer_Param (5))
             & (Database.E2e.From_LI = Integer_Param (6)),
             Where => Database.Entity_Refs.File = Integer_Param (2)
               and Database.Entity_Refs.Line = Integer_Param (3)
               and Database.Entity_Refs.Column = Integer_Param (4)),
        On_Server => True, Name => "set_entity_renames");

   Query_Set_Caller_At_Decl : constant Prepared_Statement :=
     Prepare
       (SQL_Update
            (Table => Database.Entities,
             Set   => (Database.Entities.Decl_Caller = Integer_Param (2)),
             Where => Database.Entities.Id = Integer_Param (1)),
        On_Server => True, Name => "set_caller_at_decl");

   Query_Set_Entity_Name_And_Kind : constant Prepared_Statement :=
     Prepare
       (SQL_Update
            (Table => Database.Entities,
             Set   => (Database.Entities.Name = Text_Param (2))
                & (Database.Entities.Kind = Text_Param (3)),
             Where => Database.Entities.Id = Integer_Param (1)),
        On_Server => True, Name => "set_entity_name_and_kind");

   Query_Set_Entity_Import : constant Prepared_Statement :=
     Prepare
       (SQL_Update
            (Table => Database.Entities,
             Set   => Database.Entities.Imports = Text_Param (2),
             Where => Database.Entities.Id = Integer_Param (1)),
        On_Server => True, Name => "set_entity_import");

   package VFS_To_Ids is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Integer,   --  Id in the files table
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=");
   use VFS_To_Ids;

   type Loc is record
      File_Id : Integer;
      Line    : Integer;
      Column  : Integer;
   end record;
   --  A location within a file. Within a given ALI, a location matches a
   --  single entity, even though there might potentially be multiple lines
   --  for it. We simply merge them.

   function Hash (L : Loc) return Ada.Containers.Hash_Type;
   function Hash (L : Loc) return Ada.Containers.Hash_Type is
      function Shift_Left
        (Value  : Hash_Type;
         Amount : Natural) return Hash_Type;
      pragma Import (Intrinsic, Shift_Left);

      H : Hash_Type := Hash_Type (L.File_Id);
   begin
      --  Inspired by Ada.Strings.Hash
      H := Hash_Type (L.Line) + Shift_Left (H, 6) + Shift_Left (H, 16) - H;
      H := Hash_Type (L.Column) + Shift_Left (H, 6) + Shift_Left (H, 16) - H;
      return H;
   end Hash;

   type Entity_Info is record
      Id         : Integer;   --  Id in the files table
      Known_Name : Boolean;   --  Whether the name is known
   end record;

   package Loc_To_Ids is new Ada.Containers.Hashed_Maps
     (Key_Type        => Loc,    --  entity declaration
      Element_Type    => Entity_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");
   use Loc_To_Ids;

   package Depid_To_Ids is new Ada.Containers.Vectors
     (Index_Type      => Positive,  --  index in the ALI file ("D" lines)
      Element_Type    => Integer);  --  Id in the files table
   use Depid_To_Ids;

   type Entity_Renaming is record
      Entity : Integer;              --  Id in the entities table
      File, Line, Column : Integer;  --  A reference to the renamed entity
      Kind  : E2e_Id;
      From_LI : Integer;
   end record;
   package Entity_Renaming_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Entity_Renaming);
   use Entity_Renaming_Lists;
   --  Renamings need to be handled in a separate pass, since the ALI file
   --  points to a reference of the renamed entity, which we can only resolve
   --  once we have parsed the whole ALI file.

   type LI_Info is record
      Id    : Integer;
      LI    : GNATCOLL.Projects.Library_Info;
      Stamp : Time;
   end record;
   package LI_Lists is new Ada.Containers.Doubly_Linked_Lists (LI_Info);
   use LI_Lists;

   type Line_Info is record
      Entity : Integer; --  id of the entity that encloses this line (or -1)
      Scope  : Natural; --  number of lines the entity encloses
   end record;
   type Line_Info_Array is array (Natural range <>) of Line_Info;
   type Line_Info_Array_Access is access Line_Info_Array;
   type File_Scope_Tree is record
      Lines : Line_Info_Array_Access;
      Max   : Natural := 0;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Array, Line_Info_Array_Access);

   procedure Insert
     (Lines     : in out File_Scope_Tree;
      Entity    : Integer;
      Low, High : Integer);
   --  Store scope info for a new entity

   type Scope_Tree_Array is array (Natural range <>) of File_Scope_Tree;
   type Scope_Tree_Array_Access is access all Scope_Tree_Array;
   --  A collection of scope trees, since a given LI file represents multiple
   --  source files.

   procedure Free (Trees : in out Scope_Tree_Array_Access);

   procedure Grow_As_Needed
     (Trees : in out Scope_Tree_Array_Access;
      Count : Natural);
   --  Ensures that Trees contains at least Count files.

   function Get_Caller
     (Trees      : Scope_Tree_Array_Access;
      File_Index : Integer;
      Line       : Integer) return Integer;
   --  Returns the entity id for the given file (or -1 if file is not in the
   --  list of scope trees). File_Index is the index as returned by
   --  Is_Unit_File.

   procedure Create_Database
     (Connection : access Database_Connection_Record'Class);
   --  Create the database tables and initial contents.
   --  Behavior is undefined if the database is not empty initially.

   procedure Parse_LI
     (DB                : Database_Connection;
      Tree              : Project_Tree;
      LI                : LI_Info;
      VFS_To_Id         : in out VFS_To_Ids.Map;
      Entity_Decl_To_Id : in out Loc_To_Ids.Map;
      Entity_Renamings  : in out Entity_Renaming_Lists.List);
   --  Parse the contents of a single LI file.
   --  VFS_To_Id is a local cache for the entries in the files table.
   --
   --  Entity_Decl_To_Id maps a "file|line.col" to an entity id. This is filled
   --  during a first pass, and is needed to resolve references to parent
   --  types, index types,... during the second pass. This table does not
   --  include the name of the entity, since this is unknown when seeing the
   --  xref. But while parsing a given ALI file, the location is always unique
   --  (which would be potentially false if sharing this table for multiple
   --  ALIs)
   --
   --  VFS_To_Id is a cache for source files.

   ---------------------
   -- Create_Database --
   ---------------------

   procedure Create_Database
     (Connection      : access Database_Connection_Record'Class)
   is
      Start   : Time;
   begin
      if Active (Me_Timing) then
         Start := Clock;
      end if;

      GNATCOLL.ALI.Database.Create_Database (Connection);
      Connection.Commit_Or_Rollback;

      if Active (Me_Timing) then
         Trace
           (Me_Timing,
            "Created database:" & Duration'Image (Clock - Start) & " s");
      end if;
   end Create_Database;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Lines     : in out File_Scope_Tree;
      Entity    : Integer;
      Low, High : Integer)
   is
      Tmp   : Line_Info_Array_Access;
      Scope : constant Integer := High - Low;
      Test_From, Test_To : Integer;
   begin
      if Lines.Lines = null then
         Lines.Lines := new Line_Info_Array (1 .. Integer'Max (High, 10_000));
         Lines.Max   := 0;  --  no line initialized
      elsif Lines.Lines'Last < High then
         Tmp := Lines.Lines;
         Lines.Lines := new Line_Info_Array (1 .. High * 2);
         Lines.Lines (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      --  Various cases are possible, depending where the range low..high
      --  occurs compared to the data we already know. We could take the naive
      --  approach of always reseting the array when we grow it, and always
      --  comparing the full Low..High range, but this is slower in practice.
      --
      --   |1-------max|
      --          |low-------high|
      --              reset: none,  test: low .. max,  force: max + 1 .. high
      --
      --                   |low--high|
      --              reset: max + 1 .. low - 1, test: none, force: low .. high
      --
      --      |l..h|
      --              reset: none, test: low .. high,  force: none

      if Lines.Max < High then
         Test_From := Low;

         if Low < Lines.Max then
            Lines.Lines (Lines.Max + 1 .. High) :=  --  force
              (others => (Entity => Entity, Scope => Scope));
            Test_To   := Lines.Max;
         else
            Lines.Lines (Lines.Max + 1 .. Low - 1) :=  -- reset
              (others => (Entity => -1, Scope => 0));
            Lines.Lines (Low .. High) := --  force
              (others => (Entity => Entity, Scope => Scope));
            Test_To   := Low - 1;  --  no test
         end if;
         Lines.Max := High;
      else
         --  no reset and no force
         Test_From := Low;
         Test_To   := High;
      end if;

      for Line in Test_From .. Test_To loop
         --  Override only if we have a more narrow scope (ie we are a child of
         --  the entity known at that line).

         if Lines.Lines (Line).Scope > Scope then
            Lines.Lines (Line) := (Entity => Entity, Scope => Scope);
         end if;
      end loop;
   end Insert;

   ----------
   -- Free --
   ----------

   procedure Free (Trees : in out Scope_Tree_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scope_Tree_Array, Scope_Tree_Array_Access);
   begin
      if Trees /= null then
         for T in Trees'Range loop
            Unchecked_Free (Trees (T).Lines);
         end loop;
         Unchecked_Free (Trees);
      end if;
   end Free;

   --------------------
   -- Grow_As_Needed --
   --------------------

   procedure Grow_As_Needed
     (Trees : in out Scope_Tree_Array_Access;
      Count : Natural)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scope_Tree_Array, Scope_Tree_Array_Access);
      Tmp : Scope_Tree_Array_Access;
   begin
      if Trees = null then
         Trees := new Scope_Tree_Array (1 .. Count);
      elsif Trees'Length < Count then
         Tmp := Trees;
         Trees := new Scope_Tree_Array (1 .. Count);
         Trees (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;
   end Grow_As_Needed;

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller
     (Trees      : Scope_Tree_Array_Access;
      File_Index : Integer;
      Line       : Integer) return Integer is
   begin
      if Trees = null
        or else File_Index not in Trees'Range
        or else Line > Trees (File_Index).Max
      then
         return -1;
      else
         return Trees (File_Index).Lines (Line).Entity;
      end if;
   end Get_Caller;

   --------------
   -- Parse_LI --
   --------------

   procedure Parse_LI
     (DB                : Database_Connection;
      Tree              : Project_Tree;
      LI                : LI_Info;
      VFS_To_Id         : in out VFS_To_Ids.Map;
      Entity_Decl_To_Id : in out Loc_To_Ids.Map;
      Entity_Renamings  : in out Entity_Renaming_Lists.List)
   is
      M      : Mapped_File;
      Str    : Str_Access;
      Last   : Integer;
      Index  : Integer;

      ALI_Id   : Integer := LI.Id;

      Start           : Integer;
      Current_Unit_Id : Integer := -1;
      Dep_Id          : Integer;

      D_Line_Id       : Positive := 1;
      --  Current "D" line index

      Depid_To_Id     : Depid_To_Ids.Vector;

      Unit_Files : Depid_To_Ids.Vector;
      --  Contains the list of units associated with the current ALI (these
      --  are the ids in the "files" table). This list generally only contains
      --  a few elements, so is reasonably fast.
      --  These are the files from the "U" lines (spec, body and separates).

      Scope_Trees : Scope_Tree_Array_Access;

      Current_X_File : Integer;
      --  Id (in the database) of the file for the current X section

      Current_X_File_Unit_File_Index : Integer := -1;
      --  This is set to a Natural if the Current_X_File represents a file
      --  associated with a Unit_File ("U" line") of the current LI. The exact
      --  value is used as an index in the list of scope trees.

      Xref_File, Xref_Line, Xref_Col : Integer;
      Xref_Kind : Character;
      --  The current xref, result of Get_Xref

      Xref_File_Unit_File_Index : Integer := -1;
      --  Whether the current Xref_File would return true for Is_Unit_File.

      Current_Entity : Integer;
      --  Id in "entities" table for the current entity.

      Spec_Start_Line : Integer;
      --  Start of the declaration (which might also be the completion of the
      --  declaration if a 'c' reference is found). This is used to compute the
      --  scope of the current entity.

      Body_Start_Line : Integer;
      --  The 'b' or 'c' reference for the current entity.

      procedure Skip_Spaces;
      pragma Inline (Skip_Spaces);
      --  Moves Index on the first character following the spaces.
      --  This doesn't check whether we go past the end-of-line or the last
      --  character in the file.

      procedure Skip_Word;
      pragma Inline (Skip_Word);
      --  Moves Index to the first whitespace character following the current
      --  word

      procedure Skip_To_Name_End;
      pragma Inline (Skip_To_Name_End);
      --  From the start of the name of the entity in an entity line in a X
      --  section, move Index to the first character after the name of the
      --  entity (this could be a space, or the beginning of a renaming
      --  declaration, or the '<' for the parent type,...).
      --  So Index should initially point to the first character of the name.

      procedure Skip_Instance_Info (Instance : out Unbounded_String);
      --  Skip any instantiation info "[file|line[fil2|line[...]]]".
      --  Returns the normalized description of the instance suitable for the
      --  entity_refs table.

      procedure Skip_Import_Info;
      --  Skip any information about imports, in references:
      --      65b<c,gnatcoll_munmap>22

      procedure Next_Line;
      pragma Inline (Next_Line);
      --  Moves Index to the beginning of the next line

      function Get_Natural return Natural;
      pragma Inline (Get_Natural);
      --  Read an integer at the current position, and moves Index after it.

      function Get_Char return Character;
      pragma Inline (Get_Char);
      --  Return the current character, and move forward

      function Get_Or_Create_Entity
        (Decl_File   : Integer;
         Decl_Line   : Integer;
         Decl_Column : Integer;
         Name        : String;
         Kind        : Character) return Integer;
      --  Lookup an entity at the given location. If the entity is already
      --  known in the local hash table, it is reused, otherwise it is searched
      --  in the database. If it doesn't exist there, a new entry is created
      --  using the Name and the Kind (the name is not used when searching in
      --  the local htable, since we assume there is a single entity at that
      --  location).
      --  Decl_Column can be set to -1 if the column is unknown (case of a
      --  generic instantiation in the ALI file).

      procedure Get_Ref (With_Col : Boolean := True);
      --  Parse a "file|line kind col" reference (the file is optional,
      --  and left untouched if unspecified). Sets the Xref_* variables
      --  accordingly.
      --  If With_Col is False, no "kind col" is expected, although one can be
      --  given for compatibility with further changes in ALI
      --  (and Xref_Col is set to -1 on exit).

      function Get_Ref_Or_Predefined
        (Endchar   : Character;
         Eid       : E2e_Id := -1;
         E2e_Order : Integer := 1;
         With_Col  : Boolean := True;
         Process_E2E : Boolean) return Boolean;
      --  Parse a "file|line kind col" reference, or the name of a predefined
      --  entity. After this ref or name, we expect to see Endchar.
      --  Returns False if there is an error.
      --  This inserts appropriate entries in the "e2e" table to document
      --  the relationship between the newly parsed entity and the current
      --  entity. This kind of this relationship is given by Eid. Its "order"
      --  is given by E2e_Order.
      --  If Process_E2E is false, then nothing is stored in the database, and
      --  the information is simply skipped.

      function Insert_Source_File
        (Basename    : String;
         Is_ALI_Unit : Boolean := False) return Integer;
      --  Retrieves the id for the file in the database, or create a new entry
      --  for it.
      --  Is_ALI_Unit should be true when the file is one of the units
      --  associated with the current ALI file.
      --
      --  Returns -1 if the file is not known in the project.

      procedure Process_Entity_Line (First_Pass : Boolean);
      --  Process the current line when it is an entity declaration and its
      --  references in the current file.
      --  When First_Pass is true, this skips all the entity-to-entity
      --  relationships, but stores the references in the database.
      --  If, on the other hand, First_Pass is False, then it only processes
      --  the entity-to-entity relationships and skips the references.

      procedure Process_Xref_Section (First_Pass : Boolean);
      --  Process all the xref information found in the X sections of the ALI
      --  file.
      --  See comment in Process_Entity_Line for the meaning of First_Pass.

      function Is_Unit_File (Id : Integer) return Integer;
      --  Whether the file with the given id is one of the units associated
      --  with the current ALI.

      ------------------
      -- Is_Unit_File --
      ------------------

      function Is_Unit_File (Id : Integer) return Integer is
         C : Depid_To_Ids.Cursor := Unit_Files.First;
         Index : Natural := 1;
      begin
         while Has_Element (C) loop
            if Element (C) = Id then
               return Index;
            end if;

            Index := Index + 1;
            Next (C);
         end loop;
         return -1;
      end Is_Unit_File;

      -----------------
      -- Get_Natural --
      -----------------

      function Get_Natural return Natural is
         V : Natural := 0;
      begin
         if Str (Index) not in '0' .. '9' then
            Trace (Me_Error, "Expected a natural, got "
                   & String (Str (Index .. Integer'Min (Index + 20, Last))));
            raise Program_Error;
            return 0;  --  Error in ALI file
         end if;

         loop
            V := V * 10 + (Character'Pos (Str (Index)) - Character'Pos ('0'));
            Index := Index + 1;
            exit when Index > Last
              or else Str (Index) not in '0' .. '9';
         end loop;

         return V;
      end Get_Natural;

      --------------
      -- Get_Char --
      --------------

      function Get_Char return Character is
         C : constant Character := Str (Index);
      begin
         Index := Index + 1;
         return C;
      end Get_Char;

      -------------
      -- Get_Ref --
      -------------

      procedure Get_Ref (With_Col : Boolean := True) is
      begin
         Xref_Line := Get_Natural;

         if Str (Index) = '|' then
            Xref_File := Depid_To_Id.Element (Xref_Line);
            if ALI_Contains_External_Refs then
               Xref_File_Unit_File_Index := Is_Unit_File (Xref_File);
            end if;
            Index := Index + 1;  --  Skip '|'
            Xref_Line := Get_Natural;
         end if;

         if With_Col
           or else (Str (Index) /= '['
                    and then Str (Index) /= ']'
                    and then Str (Index + 1) in '0' .. '9')
         then
            Xref_Kind := Get_Char;
            Skip_Import_Info;
            Xref_Col := Get_Natural;
         else
            Xref_Col := -1;
         end if;
      end Get_Ref;

      ---------------------------
      -- Get_Ref_Or_Predefined --
      ---------------------------

      function Get_Ref_Or_Predefined
        (Endchar   : Character;
         Eid       : E2e_Id := -1;
         E2e_Order : Integer := 1;
         With_Col  : Boolean := True;
         Process_E2E : Boolean) return Boolean
      is
         Start : constant Integer := Index;
         Name_Last : Integer;
         Is_Predefined : constant Boolean := Str (Index) not in '0' .. '9';
         Ref_Entity : Integer := -1;
         Ignored : Unbounded_String;
         pragma Unreferenced (Ignored);
      begin
         if Is_Predefined then
            --  a predefined entity
            while Str (Index) /= Endchar loop
               Index := Index + 1;
            end loop;
            Name_Last := Index - 1;

         else
            Get_Ref (With_Col => With_Col);
         end if;

         --  Within the extra entity information (parent type, index type,...)
         --  there can be information as to where an entity is instanciated.
         --  For instance, gtk-containers.ads contains:
         --     function Children return Gtk.Widget.Widget_List.GList;
         --  and the ALI file contains:
         --     310V13 Children{30|70R12[47|125]}
         --         where 47|125 is the declaration of Widget_List
         --     70R12 GList 312r37[47|125]
         --
         --  We simply discard the instance info in the extra entity info,
         --  since it is complex to store efficiently and for now we do not use
         --  it. But for the ref itself we will store in which instantiation
         --  312r37 is found, to display in tooltips.

         Skip_Instance_Info (Ignored);

         if Get_Char /= Endchar then
            if Active (Me_Error) then
               Trace (Me_Error, "Error: expected "
                      & Character'Image (Endchar) & ", got '"
                      & String
                        (Str (Index - 1 .. Integer'Min (Index + 20, Last)))
                      & "' at index" & Index'Img);
            end if;
            return False;
         end if;

         if Is_Predefined then
            if Process_E2E then
               declare
                  R : Forward_Cursor;
                  Name : aliased String := String (Str (Start .. Name_Last));
               begin
                  R.Fetch
                    (DB,
                     Query_Find_Predefined_Entity,
                     Params => (1 => +Name'Unrestricted_Access));

                  if not R.Has_Row then
                     if Active (Me_Error) then
                        Trace (Me_Error,
                               "Missing predefined entity in the database: '"
                               & Name & "' in "
                               & LI.LI.Library_File.Display_Full_Name);
                     end if;

                     Ref_Entity := DB.Insert_And_Get_PK
                       (Query_Insert_Entity,
                        Params =>
                          (1 => +Name'Unrestricted_Access,
                           2 => +'I',
                           3 => +(-1),
                           4 => +(-1),
                           5 => +(-1)),
                        PK => Database.Entities.Id);
                  else
                     Ref_Entity := R.Integer_Value (0);
                  end if;
               end;
            end if;

         else
            --  Only insert if we have the detailed info for an entity in one
            --  of the units associated with the current LI (for instance
            --  parent type info is only taken into account for these entites,
            --  for entities in other units we'll have to parse the
            --  corresponding LI). This avoids duplicates.

            if Process_E2E
              and then Current_X_File_Unit_File_Index /= -1
              and then Xref_File /= -1
            then
               Ref_Entity := Get_Or_Create_Entity
                 (Decl_File   => Xref_File,
                  Decl_Line   => Xref_Line,
                  Decl_Column => Xref_Col,
                  Name        => "",
                  Kind        => Xref_Kind);
            end if;
         end if;

         if Process_E2E
           and then Ref_Entity /= -1
         then
            DB.Execute
              (Query_Insert_E2E,
               Params => (1 => +Current_Entity,
                          2 => +Ref_Entity,
                          3 => +Eid,
                          4 => +E2e_Order,
                          5 => +ALI_Id));
         end if;

         return True;
      end Get_Ref_Or_Predefined;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line is
      begin
         while Index <= Last
           and then Str (Index) /= ASCII.LF
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 1;  --  Skip ASCII.LF
      end Next_Line;

      ------------------------
      -- Skip_Instance_Info --
      ------------------------

      procedure Skip_Instance_Info
        (Instance : out Unbounded_String)
      is
         Nesting : Natural := 0;
         Start_File  : constant Integer := Xref_File;
         Start_Line  : constant Integer := Xref_Line;
         Start_Col   : constant Integer := Xref_Col;
         Start_Index : constant Integer := Xref_File_Unit_File_Index;
      begin
         Instance := Null_Unbounded_String;

         if Str (Index) = '[' then
            while Str (Index) = '[' loop
               Index := Index + 1;
               Nesting := Nesting + 1;

               Get_Ref (With_Col => False);

               if Instance /= Null_Unbounded_String then
                  Append (Instance, ",");
               end if;
               Append (Instance, Image (Xref_File, Min_Width => 0));
               Append (Instance, '|');
               Append (Instance, Image (Xref_Line, Min_Width => 0));
            end loop;

            Index := Index + Nesting;   --  skip closing brackets
            Xref_File := Start_File;
            Xref_Line := Start_Line;
            Xref_Col  := Start_Col;
            Xref_File_Unit_File_Index := Start_Index;
         end if;
      end Skip_Instance_Info;

      ----------------------
      -- Skip_Import_Info --
      ----------------------

      procedure Skip_Import_Info is
         Start : Integer;
      begin
         if Str (Index) = '<' then
            Start := Index + 1;
            while Str (Index) /= '>' loop
               Index := Index + 1;
            end loop;
            Index := Index + 1;

            declare
               Name : aliased String :=
                 String (Str (Start .. Index - 2));
            begin
               DB.Execute
                 (Query_Set_Entity_Import,
                  Params => (1 => +Current_Entity,
                             2 => +Name'Unrestricted_Access));
            end;
         end if;
      end Skip_Import_Info;

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces is
      begin
         while Str (Index) = ' ' or else Str (Index) = ASCII.HT loop
            Index := Index + 1;
         end loop;
      end Skip_Spaces;

      ---------------
      -- Skip_Word --
      ---------------

      procedure Skip_Word is
      begin
         while Index <= Last
           and then Str (Index) /= ' '
           and then Str (Index) /= ASCII.LF
           and then Str (Index) /= ASCII.HT
         loop
            Index := Index + 1;
         end loop;
      end Skip_Word;

      ----------------------
      -- Skip_To_Name_End --
      ----------------------

      procedure Skip_To_Name_End is
      begin
         Index := Index + 1;

         if Str (Index - 1) = '"' then
            --  Operators are quoted

            while Str (Index) /= '"' loop
               Index := Index + 1;
            end loop;
            Index := Index + 1;   --  skip closing quote

         else
            --  Entity names can contain extra information, like
            --  pointed type,... So we need to extract the name
            --  itself and will store the extra information in a
            --  second step

            while Str (Index) /= ' '
              and then Str (Index) /= ASCII.LF
              and then Str (Index) /= '{'
              and then Str (Index) /= '['
              and then Str (Index) /= '<'
              and then Str (Index) /= '('
            loop
               Index := Index + 1;
            end loop;
         end if;
      end Skip_To_Name_End;

      ------------------------
      -- Insert_Source_File --
      ------------------------

      function Insert_Source_File
        (Basename    : String;
         Is_ALI_Unit : Boolean := False) return Integer
      is
         File : constant Virtual_File :=
           Tree.Create
             (Name            => +Basename,
              Use_Object_Path => False);
         Found : VFS_To_Ids.Cursor;
         Id    : Integer;
      begin
         if File = GNATCOLL.VFS.No_File then
            if Active (Me_Error) then
               Trace (Me_Error, "File not found in project: " & Basename);
            end if;
            return -1;
         end if;

         Found := VFS_To_Id.Find (File);
         if Has_Element (Found) then
            Id := Element (Found);
         else
            declare
               Name  : constant Cst_Filesystem_String_Access :=
                 File.Full_Name (Normalize => True);
               Name_A : constant Access_String := Convert (Name);
               Files : Forward_Cursor;
            begin
               Files.Fetch (DB, Query_Get_File, Params => (1 => +Name_A));

               if Files.Has_Row then
                  Id := Files.Integer_Value (0);
               else
                  declare
                     Lang : aliased String := Tree.Info (File).Language;
                  begin
                     Id := DB.Insert_And_Get_PK
                       (Query_Insert_Source_File,
                        Params => (1 => +Name_A,
                                   2 => +Lang'Unrestricted_Access),
                        PK => Database.Files.Id);
                  end;
               end if;

               VFS_To_Id.Insert (File, Id);
            end;
         end if;

         if Is_ALI_Unit then
            Unit_Files.Append (Id);
            Grow_As_Needed (Scope_Trees, Integer (Unit_Files.Length));

            --  Clear previous info known for this source file.
            --  This cannot be done with a single query when we see the
            --  create the LI file because it is possible to get
            --  duplicates otherwise:
            --  For instance, a generic instantiation ALI contains:
            --     U glib.xml_int%b        glib-xml_int.ads
            --     U glib.xml_int%s        glib-xml_int.ads
            --  In this case, we would have duplicate entries in f2f
            --  ("has ali" at least, and likely "withs" as well)
            --
            --  A similar error when a given basename is found in two
            --  different locations (s-memory.adb for instance), which
            --  can occur when overriding runtime files.

            DB.Execute (Query_Delete_File_Dep, Params => (1 => +Id));
            DB.Execute (Query_Delete_Refs, Params => (1 => +Id));
         end if;

         return Id;
      end Insert_Source_File;

      --------------------------
      -- Get_Or_Create_Entity --
      --------------------------

      function Get_Or_Create_Entity
        (Decl_File   : Integer;
         Decl_Line   : Integer;
         Decl_Column : Integer;
         Name        : String;
         Kind        : Character) return Integer
      is
         R : Forward_Cursor;
         Decl : constant Loc :=
           (File_Id => Decl_File,
            Line    => Decl_Line,
            Column  => Decl_Column);
         C        : Loc_To_Ids.Cursor;
         Info     : Entity_Info;
         Candidate : Integer := -1;
         Candidate_Is_Forward : Boolean := True;
      begin
         if Decl_Column = -1 then
            if Instances_Provide_Column then
               Trace
                 (Me_Error,
                  "The ALI parser expects instance info to contain column");
               return -1;
            end if;

            --  We don't know the column (case of instantiation information in
            --  ALI files). We do not use the local cache, since sqlite will be
            --  much more efficient to handle it.

            if Name'Length /= 0 then
               if Active (Me_Error) then
                  Trace (Me_Error,
                         "Instantiations should not document the name");
               end if;
               return -1;
            end if;

            R.Fetch
              (DB,
               Query_Find_Entity_From_Decl_No_Column,
               Params =>
                 (1 => +Decl_File,
                  2 => +Decl_Line));

            while R.Has_Row loop
               Candidate := R.Integer_Value (0);
               exit when R.Value (1) /= "";
               R.Next;
            end loop;

            if Candidate = -1 then
               --  We need to insert a forward declaration for an entity whose
               --  name and column of the declaration we do not know. We'll
               --  try to complete later.

               Trace
                 (Me_Forward, "Insert forward declaration (column unknown)");
               Candidate := DB.Insert_And_Get_PK
                 (Query_Insert_Entity,
                  Params =>
                    (1 => +Name'Unrestricted_Access,   --  empty string
                     2 => +'P',  --  unknown
                     3 => +Decl_File,
                     4 => +Decl_Line,
                     5 => +(-1)),
                  PK => Database.Entities.Id);
            end if;

            return Candidate;
         end if;

         --  It is possible that we have already seen the same
         --  entity earlier in the file. Unfortunately, duplicates
         --  happen, for instance in .gli files

         C := Entity_Decl_To_Id.Find (Decl);

         if Has_Element (C) then
            Info := Element (C);

            if Info.Known_Name         --  Do we know the entity ?
              or else Name'Length = 0  --  Or do we still have forward decl
            then
               return Info.Id;
            end if;
         end if;

         --  Either we have never seen that entity before, or we had a forward
         --  declaration (because the entity is for instance the parent of
         --  another entity, but the ALI file did not contain its name).
         --  We'll need to update the database.
         --  If we had an element in the local cache, it was for a forward
         --  declaration or we would have returned earlier. In this case, we
         --  know that in the database we will also find the forward
         --  declaration (or the local cache would have been updated), and thus
         --  we don't need to search in this case.

         if Name'Length /= 0
           or else not Has_Element (C)
         then
            R.Fetch
              (DB,
               Query_Find_Entity_From_Decl,
               Params =>
                 (1 => +Decl_File,
                  2 => +Decl_Line,
                  3 => +Decl_Column));

            while R.Has_Row loop
               if Name'Length /= 0 and then R.Value (1) = Name then
                  Candidate := R.Integer_Value (0);
                  Candidate_Is_Forward := False;
                  exit;
               elsif Name'Length = 0 and then R.Value (1) /= "" then
                  Candidate := R.Integer_Value (0);
                  Candidate_Is_Forward := False;
                  exit;
               elsif R.Value (1) = "" then
                  Candidate := R.Integer_Value (0);
                  Candidate_Is_Forward := True;
                  --  keep looking, we only found a forward declaration
               end if;

               R.Next;
            end loop;

            if Candidate = -1
              and then not Instances_Provide_Column
            then
               --  No candidate found, perhaps there is a forward declaration
               --  coming from a generic instantiation, ie without column
               --  information.

               R.Fetch
                 (DB,
                  Query_Find_Entity_From_Decl,
                  Params =>
                    (1 => +Decl_File,
                     2 => +Decl_Line,
                     3 => +(-1)));

               if R.Has_Row then
                  Candidate := R.Integer_Value (0);
                  Candidate_Is_Forward := True;
               end if;
            end if;

            if Candidate /= -1 then
               if not Candidate_Is_Forward then
                  --  We have found an entity with a known name and decl,
                  --  that's the good one.

                  Entity_Decl_To_Id.Include
                    (Decl,
                     Entity_Info'(Id         => Candidate,
                                  Known_Name => True));

               elsif Name'Length /= 0 then
                  --  We had a forward declaration in the database, we can
                  --  now update its name.
                  DB.Execute
                    (Query_Set_Entity_Name_And_Kind,
                     Params => (1 => +Candidate,
                                2 => +Name'Unrestricted_Access,
                                3 => +Kind));

                  Entity_Decl_To_Id.Include
                    (Decl,
                     Entity_Info'(Id         => Candidate,
                                  Known_Name => True));

               else
                  --  Record partial information in the local cache
                  Entity_Decl_To_Id.Insert
                    (Decl,
                     Entity_Info'(Id         => Candidate,
                                  Known_Name => False));
               end if;

               return Candidate;
            end if;
         end if;

         --  The entity was not in the database, save it. If the name is empty
         --  we are creating a forward declaration.

         if not Has_Element (C) then
            Candidate := DB.Insert_And_Get_PK
              (Query_Insert_Entity,
               Params =>
                 (1 => +Name'Unrestricted_Access,
                  2 => +Kind,
                  3 => +Decl_File,
                  4 => +Decl_Line,
                  5 => +Decl_Column),
               PK => Database.Entities.Id);

            Entity_Decl_To_Id.Insert
              (Decl,
               Entity_Info'(Id         => Candidate,
                            Known_Name => Name'Length /= 0));
            return Candidate;

         else
            return Element (C).Id;
         end if;
      end Get_Or_Create_Entity;

      --------------------------
      -- Process_Xref_Section --
      --------------------------

      procedure Process_Xref_Section (First_Pass : Boolean) is
      begin
         while Index <= Last loop
            if Str (Index) = 'X' then
               Index := Index + 2;

               --  Could be set to -1 if the file is not found in the project's
               --  sources (for instance sdefault.adb)
               Current_X_File := Depid_To_Id.Element (Get_Natural);
               Current_X_File_Unit_File_Index := Is_Unit_File (Current_X_File);

            elsif Str (Index) = '.'
              or else Str (Index) in '0' .. '9'
            then
               if Current_X_File /= -1 then
                  Process_Entity_Line (First_Pass => First_Pass);
               end if;

            else
               --  The start of another section in the ALI file
               exit;
            end if;

            Next_Line;
         end loop;
      end Process_Xref_Section;

      -------------------------
      -- Process_Entity_Line --
      -------------------------

      procedure Process_Entity_Line (First_Pass : Boolean) is
         Process_E2E    : constant Boolean := not First_Pass;
         Process_Refs   : constant Boolean := not First_Pass;
         Process_Scopes : constant Boolean := First_Pass;
         Is_Library_Level : Boolean;
         Ref_Entity : Integer;
         Name_End, Name_Start : Integer;
         Entity_Kind : Character;
         Eid : E2e_Id;
         Order : Natural := 0;
         Will_Insert_Ref : Boolean;
         Instance : Unbounded_String;
         pragma Unreferenced (Is_Library_Level);

      begin
         if Str (Index) = '.' then
            --  Same entity as before, so we do not change current entity
            Index := Index + 2;  --  First ref on that line

         else
            Get_Ref;
            Entity_Kind      := Xref_Kind;
            Is_Library_Level := Get_Char = '*';
            Name_Start       := Index;
            Skip_To_Name_End;
            Name_End         := Index - 1;

            if Process_E2E then
               --  After First_Pass, we know the entity exists, so it is safe
               --  to call Element directly.

               Current_Entity := Entity_Decl_To_Id.Element
                 ((File_Id => Current_X_File,
                   Line    => Xref_Line,
                   Column  => Xref_Col)).Id;
               Spec_Start_Line := Xref_Line;

               --  But now we also know the caller at declaration, so we can
               --  set it.

               if Current_X_File_Unit_File_Index /= -1 then
                  declare
                     Caller : constant Integer :=
                       Get_Caller (Scope_Trees,
                                   Current_X_File_Unit_File_Index,
                                   Xref_Line);
                  begin
                     if Caller /= -1
                       and then Caller /= Current_Entity
                     then
                        DB.Execute
                          (Query_Set_Caller_At_Decl,
                           Params => (1 => +Current_Entity,
                                      2 => +Caller));
                     end if;
                  end;
               end if;

            else   --  First pass, we might need to create the entity
               --  For operators, omit the quotes when inserting into the
               --  database (since that's not what references to that
               --  entity will be using anyway.

               if Str (Name_Start) = '"'
                 and then Str (Name_End) = '"'
               then
                  Name_Start := Name_Start + 1;
                  Name_End   := Name_End - 1;
               end if;

               Spec_Start_Line := Xref_Line;
               Current_Entity := Get_Or_Create_Entity
                 (Name        => String (Str (Name_Start .. Name_End)),
                  Decl_File   => Current_X_File,
                  Decl_Line   => Spec_Start_Line,
                  Decl_Column => Xref_Col,
                  Kind        => Xref_Kind);
            end if;

            Name_End := Index;

            --  Process the extra information we had (pointed type,...)

            if Str (Name_End) = '=' then
               --  First, renaming info, as in
               --     17p4 S=17:30{83|45P9} 34r10
               --  Difficulty here is that after '=' we have the location of
               --  a reference, so we need to find the corresponding entity
               --  before we can insert in the database. We'll do that once we
               --  have inserted all other refs.

               Index := Name_End + 1;
               Get_Ref;
               Name_End := Index;

               if Process_E2E then
                  Entity_Renamings.Append
                    ((Entity => Current_Entity,
                      File   => Xref_File,
                      Line   => Xref_Line,
                      Column => Xref_Col,
                      From_LI => ALI_Id,
                      Kind   => E2e_Renames));
               end if;
            end if;

            loop
               Index := Name_End + 1;
               Order := Order + 1;
               Xref_File := Current_X_File;
               Xref_File_Unit_File_Index := Current_X_File_Unit_File_Index;

               case Str (Name_End) is
                  when '[' =>
                     --  Instantiation reference, as in
                     --     5K12 G[1|3] 7r24 8r8 11r4
                     --  No column information

                     if not Get_Ref_Or_Predefined
                       (Endchar => ']',
                        Eid => E2e_Instance_Of,
                        E2e_Order => Order,
                        With_Col => False,
                        Process_E2E => Process_E2E)
                     then
                        return;
                     end if;

                  when '<' =>
                     --  Points to the parent types as in
                     --     7I9 My_Integer<integer> 8r28
                     --     9R9*My_Tagged<7|2R9><8R9> 9e69
                     --  For an array, this is the index type (can be
                     --     duplicated when there are multiple indexes)
                     --  For an overriding operation, this points to the
                     --     overridden operation.

                     case Entity_Kind is
                        when 'A' | 'a' =>
                           Eid := E2e_Has_Index;
                        when 'P' | 'p' =>
                           Eid := E2e_Overrides;
                        when others =>
                           Eid := E2e_Parent_Type;
                     end case;

                     if not Get_Ref_Or_Predefined
                       (Endchar => '>', Eid => Eid, E2e_Order => Order,
                        Process_E2E => Process_E2E)
                     then
                        return;
                     end if;

                  when '(' =>
                     --  Points to designated type or component type for array
                     --     6A9*My_Array(4I9)<3I9>
                     --  where 4I9 is component type, and 3I9 is index type

                     case Entity_Kind is
                        when 'A' | 'a' =>
                           Eid := E2e_Component_Type;
                        when 'P' | 'p' =>
                           Eid := E2e_Pointed_Type;
                        when 'G' | 'v' | 'V' | 'y' =>
                           Eid := E2e_Returns;
                        when others =>
                           if Active (Me_Error) then
                              Trace (Me_Error,
                                     "(...) for an entity of kind "
                                     & Entity_Kind'Img);
                           end if;
                           Eid := -1;
                     end case;

                     if not Get_Ref_Or_Predefined
                       (Endchar => ')', Eid => Eid, E2e_Order => Order,
                        Process_E2E => Process_E2E)
                     then
                        return;
                     end if;

                  when '{' =>
                     --  Points to ancestor type for subtypes
                     --  Points to result type for functions
                     --  Points to enum type for enumeration literal
                     --  Points to type for objects and components

                     case Entity_Kind is
                        when 'G' | 'v' | 'V' | 'y' =>
                           Eid := E2e_Returns;
                        when 'n' =>
                           Eid := E2e_From_Enumeration;
                        when others =>
                           if Is_Upper (Str (Name_End)) then
                              Eid := E2e_Parent_Type;
                           else
                              Eid := E2e_Of_Type;
                           end if;
                     end case;

                     if not Get_Ref_Or_Predefined
                       (Endchar => '}', Eid => Eid, E2e_Order => Order,
                        Process_E2E => Process_E2E)
                     then
                        return;
                     end if;

                  when ' ' =>
                     exit;

                  when ASCII.LF =>
                     --  For the next call to Next_Line
                     Index := Name_End;
                     return;

                  when others =>
                     if Active (Me_Error) then
                        Trace
                          (Me_Error, "Unexpected character in ALI: "
                           & Character'Image (Str (Name_End))
                           & " in '"
                           & String
                             (Str (Name_End
                                   .. Integer'Min (Name_End + 20, Last)))
                           & "'");
                     end if;

                     return;
               end case;

               Name_End := Index;
            end loop;

            Index := Name_End;

            Xref_File := Current_X_File;
            Xref_File_Unit_File_Index := Current_X_File_Unit_File_Index;
            Body_Start_Line := -1;
         end if;

         while Index <= Last
           and then Str (Index) /= ASCII.LF
         loop
            Skip_Spaces;
            Get_Ref;

            --  We want to store in which instantiation the ref is found,
            --  so that we can display useful info in tooltips. There can be
            --  nested instantiation information. For instance,
            --  gtk-handler.ali contains the following:
            --    X 42 gtk-marshallers.ads
            --    290P15 Handler(40|446E12) 40|778r33[545[673]]
            --    X 40 gtk-handlers.ads
            --    778p10 Cb{42|290P15[545[673]]}
            --
            --  in gtk-marshallers.ads
            --   generic
            --   package User_Return_Marshallers is        --  line 235
            --      generic
            --      package Generic_Widget_Marshaller is   --  line 289
            --         type Handler is access function     --  line 290
            --
            --  in gtk-handlers.ads
            --  generic
            --  package User_Return_Callback is
            --    package Widget_Marshaller is   --  545
            --       new Marshallers.Generic_Widget_Marshaller(..)  --  545
            --  end User_Return_Callback;
            --  package User_Return_Callback_With_Setup is  ---  671
            --    package Internal_Cb is new User_Return_Callback   --  673
            --       ...
            --    package Marshallers renames Internal_Cb.Marshallers;  678
            --    package Widget_Marshaller   --  761
            --         renames Internal_Cb.Widget_Marshaller;  --  761
            --    function To_Marshaller   --  777
            --       (Cb : Widget_Marshaller.Handler)   -- 778
            --
            --  If the user gets info for "Handler" on line 778, we want to
            --  show a tooltip that contains
            --      from instance at gtk-handlers.ads:545
            --      from instance at gtk-handlers.ads:673

            Skip_Instance_Info (Instance);
            Eid := -1;

            case Xref_Kind is
               when '>' =>
                  Eid := E2e_In_Parameter;
               when '<' =>
                  Eid := E2e_Out_Parameter;
               when '=' =>
                  Eid := E2e_In_Out_Parameter;
               when '^' =>
                  Eid := E2e_Access_Parameter;
               when 'p' | 'P' =>
                  Eid := E2e_Has_Primitive;
               when 'r' | 'm' | 'l' | 'R' | 's' | 'w' | 'i' | 'k' | 'D'
                  | 'H' | 'o' | 'x' =>
                  null;  --  real references
               when 'd' =>
                  Eid := E2e_Has_Discriminant;
               when 'z' =>
                  Eid := E2e_Is_Formal_Of;
               when 'c' =>  --  completion of spec
                  Spec_Start_Line := Xref_Line;
               when 'b' =>  --  body
                  Body_Start_Line := Xref_Line;

               --  ??? Should we look in the reference_kinds table to see what
               --  kinds mark end of scopes.
               when 'e' =>  --  end of spec
                  if Process_Scopes
                    and then Xref_File_Unit_File_Index /= -1
                  then
                     Insert (Scope_Trees (Xref_File_Unit_File_Index),
                             Entity => Current_Entity,
                             Low    => Spec_Start_Line,
                             High   => Xref_Line);
                  end if;
               when 't' =>  --  end of body
                  if Process_Scopes
                    and then Body_Start_Line /= -1
                    and then Xref_File_Unit_File_Index /= -1
                  then
                     Insert (Scope_Trees (Xref_File_Unit_File_Index),
                             Entity => Current_Entity,
                             Low    => Body_Start_Line,
                             High   => Xref_Line);
                  end if;

               when others =>
                  Trace (Me_Error, "Unknown entity kind=" & Xref_Kind'Img);
            end case;

            if Eid = -1 then
               if not Process_Refs then
                  Will_Insert_Ref := False;
               else
                  Will_Insert_Ref :=
                    not ALI_Contains_External_Refs
                    or else Xref_File_Unit_File_Index /= -1;
               end if;

               if Will_Insert_Ref then
                  declare
                     Inst : aliased String := To_String (Instance);
                     Caller : constant Integer :=
                       Get_Caller (Scope_Trees, Xref_File_Unit_File_Index,
                                   Xref_Line);
                  begin
                     if Caller = -1 then
                        DB.Execute
                          (Query_Insert_Ref,
                           Params => (1 => +Current_Entity,
                                      2 => +Xref_File,
                                      3 => +Xref_Line,
                                      4 => +Xref_Col,
                                      5 => +Xref_Kind,
                                      6 => +Inst'Unrestricted_Access));
                     else
                        DB.Execute
                          (Query_Insert_Ref_With_Caller,
                           Params => (1 => +Current_Entity,
                                      2 => +Xref_File,
                                      3 => +Xref_Line,
                                      4 => +Xref_Col,
                                      5 => +Xref_Kind,
                                      6 => +Inst'Unrestricted_Access,
                                      7 => +Caller));
                     end if;
                  end;
               end if;

            elsif Process_E2E then
               --  The reference necessarily points to the declaration of
               --  the parameter, which exists in the same ALI file (but not
               --  necessarily the same source file).

               Will_Insert_Ref := not ALI_Contains_External_Refs
                 or else Xref_File_Unit_File_Index /= -1
                 or else Current_X_File_Unit_File_Index /= -1;

               if Will_Insert_Ref then
                  begin
                     Ref_Entity := Entity_Decl_To_Id.Element
                       ((File_Id => Xref_File,
                         Line    => Xref_Line,
                         Column  => Xref_Col)).Id;
                     DB.Execute
                       (Query_Insert_E2E,
                        Params => (1 => +Current_Entity,
                                   2 => +Ref_Entity,
                                   3 => +Eid,
                                   4 => +Order,
                                   5 => +ALI_Id));
                     Order := Order + 1;
                  exception
                     when Constraint_Error =>
                        Entity_Renamings.Append
                          ((Entity => Current_Entity,
                            File   => Xref_File,
                            Line   => Xref_Line,
                            Column => Xref_Col,
                            From_LI => ALI_Id,
                            Kind   => Eid));
                  end;
               end if;
            end if;
         end loop;
      end Process_Entity_Line;

      Start_Of_X_Section : Integer;

   begin
      if Active (Me_Debug) then
         Trace (Me_Debug, "Parse LI "
                & LI.LI.Library_File.Display_Full_Name);
      end if;

      if ALI_Id = -1 then
         ALI_Id := DB.Insert_And_Get_PK
           (Query_Insert_LI_File,
            Params => (1 => +Convert
                          (LI.LI.Library_File.Full_Name (Normalize => True)),
                       2 => +LI.Stamp),
            PK => Database.Files.Id);
      else
         DB.Execute
           (Query_Update_LI_File,
            Params => (1 => +ALI_Id, 2 => +LI.Stamp));
         DB.Execute (Query_Delete_E2E_From_LI, Params => (1 => +ALI_Id));
      end if;

      M := Open_Read
        (Filename              => +LI.LI.Library_File.Full_Name.all,
         Use_Mmap_If_Available => True);
      Read (M);

      Str := Data (M);
      Last := GNATCOLL.Mmap.Last (M);
      Index := Str'First;

      loop
         Next_Line;

         if Index > Last then
            return;
         end if;

         case Str (Index) is
            when 'U' =>
               --  Describes a unit associated with the LI file

               Index := Index + 2;
               Skip_Word;  --  Skip unit name
               Skip_Spaces;
               Start := Index;
               Skip_Word;

               Current_Unit_Id := Insert_Source_File
                 (Basename    => String (Str (Start .. Index - 1)),
                  Is_ALI_Unit => True);
               if Current_Unit_Id /= -1 then
                  DB.Execute
                    (Query_Set_ALI,
                     Params => (1 => +Current_Unit_Id,
                                2 => +ALI_Id));
               end if;

            when 'W' =>
               --  Describes a "with" dependency with the last seen U line.
               --  There are two cases:
               --      W system%s  system.ads   system.ali
               --      W unchecked_deallocation%s
               --  The second line does not have ALI information.

               Index := Index + 2;
               Skip_Word;

               if Str (Index) = ASCII.LF then
                  --  second format ("unchecked_deallocation"). Nothing to do
                  null;
               else
                  Skip_Spaces;
                  Start := Index;
                  Skip_Word;

                  if Current_Unit_Id /= -1 then
                     Dep_Id := Insert_Source_File
                       (Basename => String (Str (Start .. Index - 1)));

                     if Dep_Id /= -1 then
                        DB.Execute
                          (Query_Set_File_Dep,
                           Params => (1 => +Current_Unit_Id, 2 => +Dep_Id));
                     end if;
                  end if;
               end if;

            when 'D' =>
               --  All dependencies for all units (used as indexes in xref)

               Index := Index + 2;
               Start := Index;
               Skip_Word;

               Dep_Id := Insert_Source_File
                 (Basename => String (Str (Start .. Index - 1)));

               Depid_To_Id.Set_Length (Ada.Containers.Count_Type (D_Line_Id));
               Depid_To_Id.Replace_Element
                 (Index    => D_Line_Id,
                  New_Item => Dep_Id);

               D_Line_Id := D_Line_Id + 1;

            when 'X' =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      --  Now process all 'X' sections, that contain the actual xref. This is
      --  done in two passes: first create entries in the db for all the
      --  entities, since we need to map from the location of a declaration to
      --  an id to resolve pointers to parent types, index types,...
      --  Then process the xref for each entity.

      if Str (Index) = 'X' then
         Start_Of_X_Section := Index;
         Process_Xref_Section (First_Pass => True);
         Index := Start_Of_X_Section;
         Process_Xref_Section (First_Pass => False);
      end if;

      Free (Scope_Trees);
      Close (M);
   end Parse_LI;

   ------------------------
   -- Parse_All_LI_Files --
   ------------------------

   procedure Parse_All_LI_Files
     (Self         : in out Xref_Database;
      Tree         : Project_Tree;
      Project      : Project_Type;
      Parse_Runtime_Files : Boolean := True;
      From_DB_Name : String := "";
      To_DB_Name   : String := "")
   is
      Is_Sqlite       : constant Boolean :=
        GNATCOLL.SQL.Sqlite.Is_Sqlite (Self.DB);

      Destroy_Indexes   : Boolean := False;
      --  If Destroy_Indexes is True, then some of the database indexes will
      --  be temporarily disabled and then recreated in the end. This will be
      --  faster when doing major changes, but will be slower otherwise. In any
      --  case, the index is only destroyed if actual changes take place in the
      --  database.

      Do_Analyze : Boolean := False;
      --  Whether to perform a "ANALYZE". This is really only needed the first
      --  time, since it will give a good idea of the contents of the database.
      --  This is not needed later on.
      --  ??? Perhaps once a day would be nice ?

      LI_Files          : Library_Info_Lists.List;
      LIs               : LI_Lists.List;
      VFS_To_Id         : VFS_To_Ids.Map;
      Entity_Decl_To_Id : Loc_To_Ids.Map;
      Entity_Renamings  : Entity_Renaming_Lists.List;

      procedure Resolve_Renamings;
      --  The last pass in parsing a ALI file is to resolve all renamings, now
      --  that we can convert a reference to an entity

      procedure Initialize_DB (DB : Database_Connection);
      --  Initialize the database if needed (copy from disk or create db)

      procedure Backup_DB_If_Needed
        (DB : Database_Connection; To_DB : String);
      --  Dump the database to disk if needed.

      procedure Finalize_DB (DB : Database_Connection);
      --  After parsing LI files, do the last post-processings in the database
      --  (recreate indexes, analyze,...)

      procedure Start_Transaction (DB : Database_Connection);
      --  Preparate the database for editing, once we have detected some LI
      --  files need to be updated.

      procedure Search_LI_Files_To_Update;
      --  Process the list of all LI files (Lib_Files) to detect those that
      --  need updating. This needs access to an existing database to check
      --  which files are up to date.

      procedure Parse_Files (DB : Database_Connection);
      --  Parse files that need it

      -----------------------
      -- Resolve_Renamings --
      -----------------------

      procedure Resolve_Renamings is
         C   : Entity_Renaming_Lists.Cursor := Entity_Renamings.First;
         Ren : Entity_Renaming;
         Start : Time;
      begin
         if Active (Me_Timing) then
            Start := Clock;
         end if;

         while Has_Element (C) loop
            Ren := Element (C);
            Self.DB.Execute
              (Query_Set_Entity_Renames,
               Params => (1 => +Ren.Entity,
                          2 => +Ren.File,
                          3 => +Ren.Line,
                          4 => +Ren.Column,
                          5 => +Ren.Kind,
                          6 => +Ren.From_LI));
            Next (C);
         end loop;

         if Active (Me_Timing)
           and then not Entity_Renamings.Is_Empty
         then
            Trace (Me_Timing,
                   "Processed" & Length (Entity_Renamings)'Img
                   & " incomplete refs:"
                   & Duration'Image (Clock - Start) & " s");
         end if;
      end Resolve_Renamings;

      -------------------
      -- Initialize_DB --
      -------------------

      procedure Initialize_DB (DB : Database_Connection) is
         Start : Time;
      begin
         if Is_Sqlite then
            declare
               Current_DB  : constant String :=
                 GNATCOLL.SQL.Sqlite.DB_Name (DB);
            begin
               if Is_Regular_File (Current_DB) then
                  --  If the DB already exists, don't override it
                  null;

               elsif Current_DB /= From_DB_Name
                 and then From_DB_Name /= ""
                 and then Is_Regular_File (From_DB_Name)
               then
                  if Active (Me_Timing) then
                     Start := Clock;
                  end if;

                  if not GNATCOLL.SQL.Sqlite.Backup
                    (DB1             => DB,
                     DB2             => From_DB_Name,
                     From_DB1_To_DB2 => False)
                  then
                     Trace
                       (Me_Error,
                        "Failed to copy the database from " & From_DB_Name);

                  elsif Active (Me_Timing) then
                     Trace (Me_Timing, "Copy " & From_DB_Name
                            & " to " & Current_DB & ":"
                            & Duration'Image (Clock - Start) & " s");
                  end if;

               else
                  Create_Database (DB);
                  Destroy_Indexes := True;
                  Do_Analyze := True;
               end if;
            end;
         end if;
      end Initialize_DB;

      -------------------------
      -- Backup_DB_If_Needed --
      -------------------------

      procedure Backup_DB_If_Needed
        (DB : Database_Connection; To_DB : String)
      is
         Start : Time;
      begin
         if Is_Sqlite
           and then To_DB /= ""
           and then GNATCOLL.SQL.Sqlite.DB_Name (DB) /= To_DB
         then
            if Active (Me_Timing) then
               Start := Clock;
            end if;

            if not GNATCOLL.SQL.Sqlite.Backup
              (DB1 => DB,
               DB2 => To_DB)
            then
               Trace (Me_Error, "Failed to backup the database to disk");
            elsif Active (Me_Timing) then
               Trace (Me_Timing,
                      "Backup to " & To_DB & ":"
                      & Duration'Image (Clock - Start) & " s");
            end if;
         end if;
      end Backup_DB_If_Needed;

      -----------------
      -- Finalize_DB --
      -----------------

      procedure Finalize_DB (DB : Database_Connection) is
         Start : Time;
      begin
         if Destroy_Indexes then
            if Active (Me_Timing) then
               Start := Clock;
            end if;

            DB.Execute
              ("CREATE INDEX entity_refs_entity on entity_refs(entity)");

            if Active (Me_Timing) then
               Trace (Me_Timing,
                      "CREATE INDEXes: "
                      & Duration'Image (Clock - Start) & " s");
            end if;
         end if;

         --  Do this once we have restored the indexes, to speed up the
         --  search.
         Resolve_Renamings;

         --  Need to commit before we can change the pragmas

         DB.Commit_Or_Rollback;

         if DB.Has_Pragmas then
            DB.Execute ("PRAGMA foreign_keys=ON");

            --  The default would be FULL, but we do not need to prevent
            --  against system crashes in this application.
            DB.Execute ("PRAGMA synchronous=NORMAL");

            --  The default would be DELETE, but we do not care enough about
            --  data integrity
            DB.Execute ("PRAGMA journal_mode=MEMORY");

            --  We can store temporary tables in memory
            DB.Execute ("PRAGMA temp_store=MEMORY");
         end if;

         --  Gather statistics to speed up the query optimizer. This isn't
         --  need systematically, and might take a while to generate, so we
         --  do it when the user also wanted to rebuild the index

         if Do_Analyze then
            if Active (Me_Timing) then
               Start := Clock;
            end if;

            DB.Execute ("ANALYZE");

            if Active (Me_Timing) then
               Trace
                 (Me_Timing,
                  "ANALYZE:" & Duration'Image (Clock - Start) & " s");
            end if;
         end if;
      end Finalize_DB;

      -----------------------
      -- Start_Transaction --
      -----------------------

      procedure Start_Transaction (DB : Database_Connection) is
      begin
         if DB.Has_Pragmas then
            --  Disable checks for foreign keys. This saves a bit of
            --  time when inserting the new references. At worse we could
            --  end up with an entity or a reference whose kind does not
            --  match an entry in the *_kind tables, and the xref will
            --  not show later on in query, but that's easily fixed by
            --  adding the new entry in the *_kind table (that is when
            --  the ALI file has changed format)

            DB.Execute ("PRAGMA foreign_keys=OFF");
            DB.Execute ("PRAGMA synchronous=OFF");
            DB.Execute ("PRAGMA journal_mode=MEMORY");
            DB.Execute ("PRAGMA temp_store=MEMORY");
         end if;

         DB.Automatic_Transactions (False);
         DB.Execute ("BEGIN");

         if Destroy_Indexes then
            DB.Execute ("DROP INDEX entity_refs_entity");
         end if;
      end Start_Transaction;

      -------------------------------
      -- Search_LI_Files_To_Update --
      -------------------------------

      procedure Search_LI_Files_To_Update is
         Lib_Info : Library_Info_Lists.Cursor := LI_Files.First;
         Files    : Forward_Cursor;
         LI       : LI_Info;
         File     : Virtual_File;
      begin
         while Has_Element (Lib_Info) loop
            LI.LI := Element (Lib_Info);
            File := LI.LI.Library_File;

            --  ??? Potentially slow on Windows (conversion to Time)
            LI.Stamp := File.File_Time_Stamp;
            LI.Id    := -1;  --  File unknown in the database

            Files.Fetch
              (Self.DB, Query_Get_File,
               Params => (1 => +Convert (File.Full_Name (Normalize => True))));

            if Files.Has_Row then
               if Files.Time_Value (1) = LI.Stamp then
                  LI.Id := -2;   --  Already up-to-date
               else
                  LI.Id := Files.Integer_Value (0);
               end if;
            end if;

            if LI.Id /= -2 then
               LIs.Append (LI);
            end if;

            Next (Lib_Info);
         end loop;
      end Search_LI_Files_To_Update;

      -----------------
      -- Parse_Files --
      -----------------

      procedure Parse_Files (DB : Database_Connection) is
         LI_C  : LI_Lists.Cursor := LIs.First;
         Start : Time;
         Dur   : Duration;
      begin
         if Active (Me_Timing) then
            Start := Clock;
         end if;

         while Has_Element (LI_C) loop
            Parse_LI (DB                => DB,
                      Tree              => Tree,
                      LI                => Element (LI_C),
                      VFS_To_Id         => VFS_To_Id,
                      Entity_Decl_To_Id => Entity_Decl_To_Id,
                      Entity_Renamings  => Entity_Renamings);
            Next (LI_C);
         end loop;

         if Active (Me_Timing) then
            Dur := Clock - Start;
            Trace (Me_Timing,
                   "Parsed" & LIs.Length'Img & " files:"
                   & Duration'Image (Dur / Integer (LIs.Length)) & "/file,"
                   & Dur'Img & " s");
         end if;
      end Parse_Files;

      Absolute_Start : Time;
   begin
      if Active (Me_Timing) then
         Absolute_Start := Clock;
      end if;

      Project.Library_Files
        (Recursive => True, Xrefs_Dirs => True, Including_Libraries => True,
         ALI_Ext => "^.*\.[ags]li$", List => LI_Files,
         Include_Predefined => Parse_Runtime_Files);

      if Active (Me_Timing) then
         Trace (Me_Timing,
                "Found" & Length (LI_Files)'Img & " [ags]li files");
      end if;

      Initialize_DB (Self.DB);   --  From_DB_Name -> Self.DB
      Search_LI_Files_To_Update;

      if not LIs.Is_Empty then
         --  Do we need to work in memory ?
         --    - only if using sqlite
         --    - unless DB is already in memory (in which case
         --      Initialize_DB has already initialized it)
         --    - if modifying enough LI files that dumping later does not cost
         --      more than the update on disk would

         if Is_Sqlite
           and then GNATCOLL.SQL.Sqlite.DB_Name (Self.DB) /= ":memory:"
           and then LIs.Length > Memory_Threshold
         then
            declare
               Memory_Descr : constant Database_Description :=
                 GNATCOLL.SQL.Sqlite.Setup (":memory:");
               Memory : constant Database_Connection :=
                 Memory_Descr.Build_Connection;
               Start : Time;
            begin
               Destroy_Indexes := True;

               Trace (Me_Timing, "Temporarily using an in-memory database");
               Initialize_DB (Memory);            --  Self.DB -> :memory:
               Start_Transaction (Memory);
               Parse_Files (Memory);
               Finalize_DB (Memory);

               if Active (Me_Timing) then
                  Start := Clock;
               end if;

               if not GNATCOLL.SQL.Sqlite.Backup
                 (From => Memory, To => Self.DB)  --  :memory: -> Self.DB
               then
                  Trace (Me_Error, "Failed to copy from :memory: to DB");
               elsif Active (Me_Timing) then
                  Trace (Me_Timing, "Copy from :memory: to "
                         & GNATCOLL.SQL.Sqlite.DB_Name (Self.DB) & ":"
                         & Duration'Image (Clock - Start) & " s");
               end if;
            end;

         else
            Start_Transaction (Self.DB);
            Parse_Files (Self.DB);
            Finalize_DB (Self.DB);
         end if;

         Backup_DB_If_Needed (Self.DB, To_DB_Name);  --  Self.DB -> To_DB_Name
      end if;

      if Active (Me_Timing) then
         Trace (Me_Timing, "Total time:"
                & Duration'Image (Clock - Absolute_Start) & " s");
      end if;
   end Parse_All_LI_Files;

   --------------
   -- Setup_DB --
   --------------

   procedure Setup_DB
     (Self : in out Xref_Database;
      DB   : not null access
        GNATCOLL.SQL.Exec.Database_Description_Record'Class)
   is
   begin
      Self.DB := DB.Build_Connection;
   end Setup_DB;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Xref_Database) is
   begin
      if Self.DB /= null then
         Close (Self.DB);
      end if;
   end Free;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self   : Xref_Database;
      Name   : String;
      File   : String;
      Line   : Integer := -1;
      Column : Integer := -1) return Entity_Information
   is
      R  : Forward_Cursor;
      Q  : SQL_Query;
      C  : SQL_Criteria;
      Entity : Entity_Information := No_Entity;
   begin
      --  First test whether the user has passed the location of the
      --  declaration.

      if Line /= -1 then
         C := Database.Entities.Decl_Line = Line;

         if Column /= -1 then
            C := C and Database.Entities.Decl_Column = Column;
         end if;
      end if;

      Q := SQL_Select
        (Database.Entities.Id,
         From  => Database.Entities & Database.Files,
         Where => Database.Files.Id = Database.Entities.Decl_File
           and Database.Entities.Name = Text_Param (1)
           and Like (Database.Files.Path, '%' & File)
         and C,
        Distinct => True,
        Limit    => 1);

      R.Fetch
        (Self.DB, Q,
         Params => (1 => +Name'Unrestricted_Access));

      if not R.Has_Row then
         --  Else check whether we have a matching reference

         C := No_Criteria;

         if Line /= -1 then
            C := Database.Entity_Refs.Line = Line;

            if Column /= -1 then
               C := C and Database.Entity_Refs.Column = Column;
            end if;
         end if;

         Q := SQL_Select
           (Database.Entity_Refs.Entity,
            From => Database.Entity_Refs & Database.Entities & Database.Files,
            Where => Database.Entity_Refs.Entity = Database.Entities.Id
            and Database.Entity_Refs.File = Database.Files.Id
            and Database.Entities.Name = Text_Param (1)
            and Like (Database.Files.Path, '%' & File)
            and C,
            Distinct => True,
            Limit    => 1);

         R.Fetch
           (Self.DB, Q,
            Params => (1 => +Name'Unrestricted_Access));
      end if;

      if R.Has_Row then
         Entity := (Id => R.Integer_Value (0));
         R.Next;

         if R.Has_Row then
            --  Overloaded entity found
            Entity := No_Entity;
         end if;
      end if;

      return Entity;
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self   : Xref_Database;
      Name   : String;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer := -1;
      Column : Integer := -1) return Entity_Information is
   begin
      return Get_Entity (Self, Name, File.Display_Full_Name, Line, Column);
   end Get_Entity;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : Base_Cursor) return Boolean is
   begin
      return Self.DBCursor.Has_Row;
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (Self : in out Base_Cursor) is
   begin
      Self.DBCursor.Next;
   end Next;

   -------------
   -- Element --
   -------------

   function Element (Self : References_Cursor) return Entity_Reference is
      Scope : Entity_Information := No_Entity;
   begin
      if not Self.DBCursor.Is_Null (5) then
         Scope := (Id => Self.DBCursor.Integer_Value (5));
      end if;

      return Entity_Reference'
        (File   => Create (+Self.DBCursor.Value (1)),
         Line   => Self.DBCursor.Integer_Value (2),
         Column => Self.DBCursor.Integer_Value (3),
         Kind   => To_Unbounded_String (Self.DBCursor.Value (4)),
         Scope  => Scope);
   end Element;

   ----------------
   -- References --
   ----------------

   function References
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return References_Cursor
   is
      Curs : References_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         SQL_Union
           (SQL_Select
              (Database.Files.Id
               &   Database.Files.Path
                 & Database.Entities.Decl_Line
                 & Database.Entities.Decl_Column
                 & Expression ("declaration")
                 & Database.Entities.Decl_Caller,
               From => Database.Entities & Database.Files,
               Where => Database.Entities.Decl_File = Database.Files.Id
                 and Database.Entities.Id = Integer_Param (1)),

            SQL_Select
              (Database.Files.Id
                 & Database.Files.Path
                 & Database.Entity_Refs.Line
                 & Database.Entity_Refs.Column
                 & Database.Reference_Kinds.Display
                 & Database.Entity_Refs.Caller,
               From => Database.Entity_Refs & Database.Files
                 & Database.Reference_Kinds,
               Where => Database.Entity_Refs.File = Database.Files.Id
                 and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                 and Database.Reference_Kinds.Is_Real
                 and Database.Entity_Refs.Entity = Integer_Param (1)),

            Order_By => Database.Files.Path
              & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
            Distinct => True),

         Params => (1 => +Entity.Id));
      return Curs;
   end References;

   -----------------
   -- Declaration --
   -----------------

   function Declaration
     (Xref   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Declaration
   is
      Curs : Forward_Cursor;
      Scope : Entity_Information := No_Entity;
   begin
      Curs.Fetch
        (Xref.DB,
         SQL_Select
           (Database.Entities.Name
            & Database.Files.Path
            & Database.Entities.Decl_Line
            & Database.Entities.Decl_Column
            & Database.Entities.Decl_Caller,
            From => Database.Entities & Database.Files,
            Where => Database.Entities.Decl_File = Database.Files.Id
            and Database.Entities.Id = Integer_Param (1)),
         Params => (1 => +Entity.Id));

      if Curs.Has_Row then
         if not Curs.Is_Null (4) then
            Scope := (Id => Curs.Integer_Value (4));
         end if;

         return (Name => To_Unbounded_String (Curs.Value (0)),
                 Location => (File   => Create (+Curs.Value (1)),
                              Line   => Curs.Integer_Value (2),
                              Column => Curs.Integer_Value (3),
                              Kind   => To_Unbounded_String ("declaration"),
                              Scope  => Scope));
      else
         return No_Entity_Declaration;
      end if;
   end Declaration;

   -------------
   -- Element --
   -------------

   function Element (Self : Entities_Cursor) return Entity_Information is
   begin
      return Entity_Information'(Id => Integer_Value (Self.DBCursor, 0));
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Self : Parameters_Cursor) return Parameter_Information is
      Kind : Parameter_Kind;
   begin
      case Integer_Value (Self.DBCursor, 1) is
         when E2e_In_Parameter     => Kind := In_Parameter;
         when E2e_Out_Parameter    => Kind := Out_Parameter;
         when E2e_In_Out_Parameter => Kind := In_Out_Parameter;
         when E2e_Access_Parameter => Kind := Access_Parameter;
         when others               => Kind := In_Parameter;
      end case;

      return Parameter_Information'
        (Parameter => (Id => Integer_Value (Self.DBCursor, 0)),
         Kind      => Kind);
   end Element;

   ----------------
   -- Parameters --
   ----------------

   function Parameters
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Parameters_Cursor
   is
      Curs : Parameters_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         SQL_Select
           (Database.E2e.Toentity
            & Database.E2e.Kind,
            From => Database.E2e,
            Where => Database.E2e.fromentity = Integer_Param (1)
                and (Database.E2e.Kind = E2e_In_Parameter
                     or Database.E2e.Kind = E2e_In_Out_Parameter
                     or Database.E2e.Kind = E2e_Out_Parameter
                     or Database.E2e.Kind = E2e_Access_Parameter),
            Order_By => Database.E2e.Order_By,
            Distinct => True),

         Params => (1 => +Entity.Id));
      return Curs;
   end Parameters;

end GNATCOLL.ALI;
