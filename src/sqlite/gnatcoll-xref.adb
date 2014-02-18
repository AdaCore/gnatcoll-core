------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2014, AdaCore                     --
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;
with GNATCOLL.Xref.Database;  use GNATCOLL.Xref.Database;
with GNATCOLL.Mmap;           use GNATCOLL.Mmap;
with GNATCOLL.SQL;            use GNATCOLL.SQL;
with GNATCOLL.SQL.Inspect;    use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

package body GNATCOLL.Xref is
   use Library_Info_Lists;

   Me_Error   : constant Trace_Handle := Create ("ENTITIES.ERROR");
   Me_Parsing : constant Trace_Handle := Create ("ENTITIES.PARSING");
   Me_Debug   : constant Trace_Handle := Create ("ENTITIES.DEBUG", Off);
   Me_Forward : constant Trace_Handle := Create ("ENTITIES.FORWARD", Off);
   Me_Timing  : constant Trace_Handle := Create ("ENTITIES.TIMING");

   Me_Use_WAL : constant Trace_Handle := Create ("ENTITIES.USE_WAL", On);
   --  Whether to use WAL journaling

   Me_Commit_Before_Indexes : constant Trace_Handle :=
      Create ("ENTITIES.COMMIT_BEFORE_INDEXES", On);
   --  If set, a COMMIT is done before we recreate the indexes, so that GPS
   --  can still making (slow) queries immediately.

   Schema_Version : constant Integer := 3;
   --  The current version of the database schema.
   --  Actual databases must match the version number exactly.
   --   2: added files.project
   --   3: initialized files with a default project (this came later)

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

   Memory_Threshold : constant Ada.Containers.Count_Type := 150_000;
   --  Number of LI files to update after which we will use a temporary
   --  in-memory copy of the database. This is computed as such:
   --    on the GPS project, it takes about .0230s per LI file when db is in
   --    memory; but we need 5.822s to recreate the indexes and about 6s to
   --    dump the database from memory to the disk.
   --    When working directly on the disk, it takes about .082s per file,
   --    with no additional cost.
   --  This gives a threshold of about 190 for my machine.

   Indexes_Threshold : constant Ada.Containers.Count_Type := 190;
   --  When reloading more than that many LI files, we first destroy the
   --  indexes and then recreate them

   Column_Tolerance : constant Natural := 80;
   --  Tolerance in columns when looking for approximate entity declarations.

   No_Project_Id : constant Integer := -2;
   --  Id of 'no project' in the files table

   type Access_String is access constant String;

   N_Files2 : aliased String := "f2";
   Files2 : T_Files (N_Files2'Access);

   N_Files3 : aliased String := "f3";
   Files3 : T_Files (N_Files3'Access);

   N_Entities2 : aliased String := "e2";
   Entities2 : T_Entities (N_Entities2'Access);

   N_E2E2 : aliased String := "e2e2";
   E2E2 : T_E2e (N_E2E2'Access);

   Query_Get_File : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Files.Id
                & Database.Files.Stamp
                & Database.Files.Language,
             From => Database.Files,
             Where => Database.Files.Path = Text_Param (1),
             Limit => 1),
        On_Server => True, Name => "get_file");
   --  Retrieve the info for a file given its path. Should only be used for
   --  project files and ALI files.

   Query_Get_File_And_Project : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (Database.Files.Id
           & Database.Files.Stamp
           & Database.Files.Language,
           From => Database.Files & Files3,
           Where => Database.Files.Path = Text_Param (1)
           and Database.Files.Project = Integer_Param (2),
           Limit => 1),
        On_Server => True, Name => "get_file_and_project");
   --  Retrieve the info for a source file given its path and its project

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

   Query_Insert_Project_File : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
          ((Database.Files.Path = Text_Param (1))
           & (Database.Files.Language = "project")),
        On_Server => True, Name => "insert_project_file");

   Query_Insert_Source_File : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Files.Path = Text_Param (1))
             & (Database.Files.Language = Text_Param (2))
             & (Database.Files.Project = Integer_Param (3))),
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

   Query_Get_ALI : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Files.Path & Database.Files.Stamp,
             From => Database.Files & Files2 & Database.F2f,
             Where => Files2.Path = Text_Param (1)
                and Database.F2f.Fromfile = Files2.Id
                and Database.F2f.Tofile = Database.Files.Id
                and Database.F2f.Kind = F2f_Has_Ali),
        On_Server => False, Name => "li_from_source");

   Query_Insert_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entities.Name = Text_Param (1))
             & (Database.Entities.Kind = Text_Param (2))
             & (Database.Entities.Decl_File = Integer_Param (3))
             & (Database.Entities.Decl_Line = Integer_Param (4))
             & (Database.Entities.Decl_Column = Integer_Param (5))
             & (Database.Entities.Is_Global = Boolean_Param (6))
             & (Database.Entities.Is_Static_Local = Boolean_Param (7))),
        On_Server => True, Name => "insert_entity");
   Query_Insert_Entity_With_Mangled : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entities.Name = Text_Param (1))
             & (Database.Entities.Kind = Text_Param (2))
             & (Database.Entities.Decl_File = Integer_Param (3))
             & (Database.Entities.Decl_Line = Integer_Param (4))
             & (Database.Entities.Decl_Column = Integer_Param (5))
             & (Database.Entities.Mangled_Name = Text_Param (6))
             & (Database.Entities.Exported = True)
             & (Database.Entities.Is_Global = Boolean_Param (7))
             & (Database.Entities.Is_Static_Local = Boolean_Param (8))),
        On_Server => True, Name => "insert_entity_with_mangled");

   Query_Set_Entity_Mangled_Name : constant Prepared_Statement :=
     Prepare
       (SQL_Update
        (Table => Database.Entities,
         Set   => (Database.Entities.Mangled_Name = Text_Param (2))
            & (Database.Entities.Exported = Boolean_Param (3)),
         Where => Database.Entities.Id = Integer_Param (1)),
        On_Server => True, Name => "set_mangled");

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
             & (Database.E2e.Order_By = Integer_Param (4))),
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
             & (Database.E2e.Kind = Integer_Param (5)),
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
                & (Database.Entities.Kind = Text_Param (3))
                & (Database.Entities.Is_Global = Boolean_Param (4))
                & (Database.Entities.Is_Static_Local = Boolean_Param (5)),
             Where => Database.Entities.Id = Integer_Param (1)),
        On_Server => True, Name => "set_entity_name_and_kind");

   Query_Mangled_Name : constant Prepared_Statement := Prepare
     (SQL_Select
      (Database.Entities.Mangled_Name,
       From => Database.Entities,
       Where => Database.Entities.Id = Integer_Param (1)),
      On_Server => False, Name => "mangled_name");

   Q_Parameters_Toentity : constant := 0;
   Q_Parameters_Kind     : constant := 1;
   Query_Parameters : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (To_List
                 ((Q_Parameters_Toentity => +Database.E2e.Toentity,
                   Q_Parameters_Kind     => +Database.E2e.Kind)),
             From => Database.E2e,
             Where => Database.E2e.fromentity = Integer_Param (1)
                and (Database.E2e.Kind = E2e_In_Parameter
                   or Database.E2e.Kind = E2e_In_Out_Parameter
                   or Database.E2e.Kind = E2e_Out_Parameter
                   or Database.E2e.Kind = E2e_Access_Parameter),
            Order_By => Database.E2e.Order_By,
             Distinct => True),
        On_Server => False, Name => "parameters");
   --  Retrieve the list of parameters for the entity in $1.
   --  Cannot be prepared because there are risks of concurrent calls.

   Entities2_Fields : constant SQL_Field_List :=
     Entities2.Id
     & Entities2.Name
     & Entities2.Decl_Line
     & Entities2.Decl_Column;

   Query_Parameter_Of : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Entities2_Fields,
             From => Entities2 & Database.E2e,
             Where => Database.E2e.toEntity = Integer_Param (1)
               and (Database.E2e.Kind = E2e_In_Parameter
                  or Database.E2e.Kind = E2e_In_Out_Parameter
                  or Database.E2e.Kind = E2e_Out_Parameter
                  or Database.E2e.Kind = E2e_Access_Parameter)
               and Database.E2e.fromEntity = Entities2.Id),
        On_Server => False, Name => "parameter_of");

   Query_E2E_From : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Entities2_Fields,
             From => Entities2 & Database.E2e,
             Where => Database.E2e.fromEntity = Integer_Param (1)
             and Database.E2e.Kind = Integer_Param (2)
             and Database.E2e.toEntity = Entities2.Id,
             Order_By =>
               Entities2.Name & Entities2.Decl_Line & Entities2.Decl_Column,
             Distinct => True),
        On_Server => False, Name => "e2e_from");
   --  Cannot be prepared because there are risks of concurrent calls.

   Query_E2E_To : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Entities2_Fields,
             From => Entities2 & Database.E2e,
             Where => Database.E2e.toEntity = Integer_Param (1)
             and Database.E2e.Kind = Integer_Param (2)
             and Database.E2e.fromEntity = Entities2.Id,
             Order_By =>
               Entities2.Name & Entities2.Decl_Line & Entities2.Decl_Column,
             Distinct => True),
        On_Server => False, Name => "e2e_to");
   --  Cannot be prepared because there are risks of concurrent calls.

   Query_Non_Inherited_Methods : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (Entities2_Fields,
           From => Entities2 & Database.E2e,
           Where => Database.E2e.fromEntity = Integer_Param (1)
           and Database.E2e.Kind = E2e_Has_Primitive
           and Database.E2e.toEntity = Entities2.Id

           --   Not a primitive of one of the parent types
           and SQL_Not_In
             (Database.E2e.toEntity,
              SQL_Select
                (Database.E2e.toEntity,
                 From  => Database.E2e,
                 Where => SQL_In
                   (Database.E2e.fromEntity,

                    --  compute the parent types (this is Query_E2e_From
                    SQL_Select
                      (Database.E2e.toEntity,
                       From => Database.E2e,
                       Where => Database.E2e.fromEntity = Integer_Param (1)
                       and Database.E2e.Kind = E2e_Parent_Type))

                 and Database.E2e.Kind = E2e_Has_Primitive)),
           Order_By =>
             Entities2.Name & Entities2.Decl_Line & Entities2.Decl_Column,
           Distinct => True),
        On_Server => False, Name => "non_inherited_methods");
   --  Cannot be prepared because there are risks of concurrent calls.

   Query_Fields : constant Prepared_Statement :=
     Prepare
       (SQL_Select
           (Database.Entities.Id
            & Database.Entities.Name
            & Database.Entities.Decl_Line
            & Database.Entities.Decl_Column,
            From => Database.Entities & Database.Entity_Kinds,
            Where => Database.Entities.Decl_Caller = Integer_Param (1)
            and Database.Entities.Kind = Database.Entity_Kinds.Id
            and Database.Entity_Kinds.Is_Subprogram = False
            and Database.Entity_Kinds.Is_Type = False
            and SQL_Not_In
              (Database.Entities.Id,
               SQL_Select
                 (Database.E2e.toEntity,
                  From => Database.E2e,
                  Where => Database.E2e.Kind = E2e_Has_Discriminant
                    and Database.E2e.fromEntity = Integer_Param (1))),
            Order_By =>
             Database.Entities.Name & Database.Entities.Decl_Line),
        On_Server => False, Name => "fields");
      --  We need to also omit subprograms, which in some languages like
      --  Java and C++ are declared within the scope of the class.

   Query_Overriding_Parameters : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Entities2_Fields,
             From => Entities2     --  the parameters info
             & Entities   --  the original parameter
             & Database.E2e & E2E2,
             Where =>
               --  Output parameter must have the same name as original
             Entities2.Name = Entities.Name
             and Entities.Id = Integer_Param (1)

               --  Entities3 must be the overriding subprograms
             and Database.E2e.toEntity = Integer_Param (2)
             and Database.E2e.Kind = E2e_Overrides
             and Database.E2e.fromEntity = E2E2.fromEntity

             --  The subprograms of those overriding subprograms
             and (E2E2.Kind = E2e_In_Parameter
               or E2E2.Kind = E2e_In_Out_Parameter
               or E2E2.Kind = E2e_Out_Parameter
               or E2E2.Kind = E2e_Access_Parameter)
             and Entities2.Id = E2E2.toEntity,

             Distinct => True),
        On_Server => False, Name => "overriding_params");

   Query_Overridden_Parameters : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Entities2_Fields,
             From => Entities2     --  the parameters info
             & Entities   --  the original parameter
             & Database.E2e & E2E2,
             Where =>
               --  Output parameter must have the same name as original
             Entities2.Name = Entities.Name
             and Entities.Id = Integer_Param (1)

               --  Entities3 must be the overriding subprograms
             and Database.E2e.fromEntity = Integer_Param (2)
             and Database.E2e.Kind = E2e_Overrides
             and Database.E2e.toEntity = E2E2.fromEntity

             --  The subprograms of those overriding subprograms
             and (E2E2.Kind = E2e_In_Parameter
               or E2E2.Kind = E2e_In_Out_Parameter
               or E2E2.Kind = E2e_Out_Parameter
               or E2E2.Kind = E2e_Access_Parameter)
             and Entities2.Id = E2E2.toEntity,

             Distinct => True),
        On_Server => False, Name => "overridden_params");

   Q_Decl_Name    : constant := 0;
   Q_Decl_File    : constant := 1;
   Q_Decl_Line    : constant := 2;
   Q_Decl_Column  : constant := 3;
   Q_Decl_Caller  : constant := 4;
   Q_Decl_Kind    : constant := 5;
   Q_Decl_Is_Subp : constant := 6;
   Q_Decl_Is_Cont : constant := 7;
   Q_Decl_Is_Abst : constant := 8;
   Q_Decl_Is_Generic : constant := 9;
   Q_Decl_Full_Decl  : constant := 10;
   Q_Decl_Is_Access  : constant := 11;
   Q_Decl_Is_Type    : constant := 12;
   Q_Decl_Is_Array   : constant := 13;
   Q_Decl_Is_Printable_In_Gdb : constant := 14;
   Q_Decl_Is_Global  : constant := 15;
   Q_Decl_Is_Static_Local : constant := 16;
   Q_Decl_Has_Methods : constant := 17;
   Q_Decl_Project     : constant := 18;
   Query_Declaration : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (To_List
                 ((Q_Decl_Name        => +Database.Entities.Name,
                   Q_Decl_File        => +Database.Files.Path,
                   Q_Decl_Line        => +Database.Entities.Decl_Line,
                   Q_Decl_Column      => +Database.Entities.Decl_Column,
                   Q_Decl_Caller      => +Database.Entities.Decl_Caller,
                   Q_Decl_Kind        => +Database.Entity_Kinds.Display,
                   Q_Decl_Is_Subp     => +Database.Entity_Kinds.Is_Subprogram,
                   Q_Decl_Is_Cont     => +Database.Entity_Kinds.Is_Container,
                   Q_Decl_Is_Abst     => +Database.Entity_Kinds.Is_Abstract,
                   Q_Decl_Is_Generic  => +Database.Entity_Kinds.Is_Generic,
                   Q_Decl_Is_Access   => +Database.Entity_Kinds.Is_Access,
                   Q_Decl_Is_Type     => +Database.Entity_Kinds.Is_Type,
                   Q_Decl_Is_Array    => +Database.Entity_Kinds.Is_Array,
                   Q_Decl_Is_Global   => +Database.Entities.Is_Global,
                   Q_Decl_Has_Methods => +Database.Entity_Kinds.Has_Methods,
                   Q_Decl_Project     => +Files2.Path,
                   Q_Decl_Is_Static_Local     =>
                      +Database.Entities.Is_Static_Local,
                   Q_Decl_Is_Printable_In_Gdb =>
                      +Database.Entity_Kinds.Is_Printable_In_Gdb,
                   Q_Decl_Full_Decl           =>
                      +Database.Entity_Kinds.Body_Is_Full_Declaration)),
             From => Database.Entities
                & Database.Files
                & Database.Entity_Kinds
                & Files2,
             Where => Database.Entities.Decl_File = Database.Files.Id
                and Database.Entities.Kind = Database.Entity_Kinds.Id
                and Database.Entities.Id = Integer_Param (1)
                and Database.Files.Project = Files2.Id),
        On_Server => True, Name => "declaration");
   --  Can be prepared because a single row is read so there is no risk of
   --  concurrent calls.

   Q_Ref_File_Id : constant := 0;
   Q_Ref_File    : constant := 1;
   Q_Ref_Line    : constant := 2;
   Q_Ref_Col     : constant := 3;
   Q_Ref_Kind    : constant := 4;
   Q_Ref_Caller  : constant := 5;
   Q_Ref_Entity  : constant := 6;  --  id of the ref'ed entity
   Q_Ref_Kind_Id : constant := 7;
   Q_Ref_Is_End_Of_Scope : constant := 8;
   Q_Ref_Project : constant := 9;
   F_References_Decl : constant SQL_Field_List := To_List
      ((Q_Ref_File_Id => +Database.Files.Id,
        Q_Ref_File    => +Database.Files.Path,
        Q_Ref_Line    => +Database.Entities.Decl_Line,
        Q_Ref_Col     => +Database.Entities.Decl_Column,
        Q_Ref_Kind    => +Expression (Reference_Kind_Declaration),
        Q_Ref_Caller  => +Database.Entities.Decl_Caller,
        Q_Ref_Entity  => +Database.Entities.Id,
        Q_Ref_Kind_Id =>
           +Expression ("" & Kind_Id_Declaration),
        Q_Ref_Is_End_Of_Scope => +Expression (False),
        Q_Ref_Project => +Files3.Path));
   F_References : constant SQL_Field_List := To_List
      ((Q_Ref_File_Id => +Database.Files.Id,
        Q_Ref_File    => +Database.Files.Path,
        Q_Ref_Line    => +Database.Entity_Refs.Line,
        Q_Ref_Col     => +Database.Entity_Refs.Column,
        Q_Ref_Kind    => +Database.Reference_Kinds.Display,
        Q_Ref_Caller  => +Database.Entity_Refs.Caller,
        Q_Ref_Entity  => +Database.Entity_Refs.Entity,
        Q_Ref_Kind_Id => +Database.Reference_Kinds.Id,
        Q_Ref_Is_End_Of_Scope => +Database.Reference_Kinds.Is_End,
        Q_Ref_Project => +Files3.Path));
   Q_References : constant Prepared_Statement :=
     Prepare
       (SQL_Union
            (SQL_Select
               (F_References_Decl,
                From => Database.Entities & Database.Files & Files3,
                Where => Database.Entities.Decl_File = Database.Files.Id
                and Database.Files.Project = Files3.Id
                and Database.Entities.Id = Integer_Param (1)),
             SQL_Select
               (F_References,
                From => Database.Entity_Refs & Database.Files
                  & Database.Reference_Kinds & Files3,
                Where => Database.Entity_Refs.File = Database.Files.Id
                and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                and Database.Reference_Kinds.Is_Real
                and Database.Files.Project = Files3.Id
                and Database.Entity_Refs.Entity = Integer_Param (1)),
             Order_By => Database.Files.Path
                & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
             Distinct => True),
        On_Server => False, Name => "references");
   --  Cannot be prepared because there are risks of concurrent calls.

   Q_File_References_Decl_By_Loc : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (F_References_Decl,
           From => Database.Entities & Database.Files & Files3,
           Where => Database.Entities.Decl_File = Database.Files.Id
              and Database.Files.Project = Files3.Id
              and Database.Files.Path = Text_Param (1),
           Order_By =>
             Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "file_declarations");
   Q_File_References_And_Kind_By_Loc : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (F_References,
           From => Database.Entity_Refs & Database.Files & Files3
             & Database.Reference_Kinds,
           Where => Database.Entity_Refs.File = Database.Files.Id
           and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
           and Database.Files.Project = Files3.Id
           and Database.Reference_Kinds.Is_Real
           and Database.Reference_Kinds.Display = Text_Param (2)
           and Database.Files.Path = Text_Param (1),
           Order_By =>
             Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "file_references_and_kind_by_loc");
   Q_File_References_And_Kind_By_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (F_References,
           From => Database.Entity_Refs & Database.Files & Files3
             & Database.Reference_Kinds,
           Where => Database.Entity_Refs.File = Database.Files.Id
           and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
           and Database.Reference_Kinds.Is_Real
           and Database.Reference_Kinds.Display = Text_Param (2)
           and Database.Files.Project = Files3.Id
           and Database.Files.Path = Text_Param (1),
           Order_By => Database.Entity_Refs.Entity
             & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "file_references_and_kind_by_entity");
   Q_File_References_By_Loc : constant Prepared_Statement :=
     Prepare
       (SQL_Union
            (SQL_Select
               (F_References_Decl,
                From => Database.Entities & Database.Files & Files3,
                Where => Database.Entities.Decl_File = Database.Files.Id
                and Database.Files.Project = Files3.Id
                and Database.Files.Path = Text_Param (1)),
             SQL_Select
               (F_References,
                From => Database.Entity_Refs & Database.Files & Files3
                  & Database.Reference_Kinds,
                Where => Database.Entity_Refs.File = Database.Files.Id
                and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                and Database.Files.Project = Files3.Id
                and Database.Reference_Kinds.Is_Real
                and Database.Files.Path = Text_Param (1)),
             Order_By =>
                Database.Entity_Refs.Line & Database.Entity_Refs.Column,
             Distinct => True),
        On_Server => False, Name => "file_references_by_loc");
   Q_File_References_By_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Union
            (SQL_Select
               (F_References_Decl,
                From => Database.Entities & Database.Files & Files3,
                Where => Database.Entities.Decl_File = Database.Files.Id
                and Database.Files.Project = Files3.Id
                and Database.Files.Path = Text_Param (1)),
             SQL_Select
               (F_References,
                From => Database.Entity_Refs & Database.Files & Files3
                  & Database.Reference_Kinds,
                Where => Database.Entity_Refs.File = Database.Files.Id
                and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                and Database.Files.Project = Files3.Id
                and Database.Reference_Kinds.Is_Real
                and Database.Files.Path = Text_Param (1)),
             Order_By => Database.Entity_Refs.Entity
                & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
             Distinct => True),
        On_Server => False, Name => "file_references_by_entity");

   Q_Calls : constant Prepared_Statement :=
     Prepare
       (SQL_Union
           (SQL_Select
              (Database.Entities.Id
                 & Database.Entities.Name
                 & Database.Entities.Decl_Line
                 & Database.Entities.Decl_Column,
               From => Database.Entity_Refs
                 & Database.Reference_Kinds
                 & Database.Entities,
               Where => Database.Entity_Refs.Caller = Integer_Param (1)
                 and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                 and Database.Reference_Kinds.Is_Real = True
                 and Database.Entities.Id = Database.Entity_Refs.Entity
                 and Database.Entity_Refs.Entity /= Integer_Param (1)),

            SQL_Select
              (Database.Entities.Id
                 & Database.Entities.Name
                 & Database.Entities.Decl_Line
                 & Database.Entities.Decl_Column,
               From => Database.Entities,
               Where => Database.Entities.Decl_Caller = Integer_Param (1)
                 and Database.Entities.Id /= Integer_Param (1)),

            Order_By =>
              Database.Entities.Name
              & Database.Entities.Decl_Line,
           Distinct => True),
        On_Server => False, Name => "calls");

   Q_End_Of_Spec : constant Prepared_Statement :=
     Prepare
       (SQL_Select
               (To_List
                 ((0    => +Database.Entity_Refs.Line,
                   1     => +Database.Entity_Refs.Column)),
             From => Database.Entity_Refs,
             Where => Database.Entity_Refs.Kind = "e"
             and Database.Entity_Refs.Entity = Integer_Param (1)),
        On_Server => True, Name => "end_of_spec");
   --  Can be prepared because a single row is read so there is no risk of
   --  concurrent calls.

   Q_References_And_Kind : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (To_List
                 ((Q_Ref_File_Id => +Database.Files.Id,
                   Q_Ref_File    => +Database.Files.Path,
                   Q_Ref_Line    => +Database.Entity_Refs.Line,
                   Q_Ref_Col     => +Database.Entity_Refs.Column,
                   Q_Ref_Kind    => +Database.Reference_Kinds.Display,
                   Q_Ref_Caller  => +Database.Entity_Refs.Caller,
                   Q_Ref_Entity  => +Database.Entity_Refs.Entity,
                   Q_Ref_Kind_Id => +Database.Reference_Kinds.Id,
                   Q_Ref_Is_End_Of_Scope =>
                     +Database.Reference_Kinds.Is_End)),
             From => Database.Entity_Refs & Database.Files
             & Database.Reference_Kinds,
             Where => Database.Entity_Refs.File = Database.Files.Id
             and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
             and Database.Reference_Kinds.Id = Text_Param (2)
             and Database.Entity_Refs.Entity = Integer_Param (1),

             Order_By => Database.Files.Path
             & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
             Distinct => True),
        On_Server => False, Name => "references_and_kind");
   pragma Unreferenced (Q_References_And_Kind);
   --  Cannot be prepared because there are risks of concurrent calls.

   Q_References_No_Implicit : constant Prepared_Statement :=
     Prepare
       (SQL_Union
          (SQL_Select
             (F_References_Decl,
              From => Database.Entities & Database.Files & Files3,
              Where => Database.Entities.Decl_File = Database.Files.Id
              and Database.Files.Project = Files3.Id
              and Database.Entities.Id = Integer_Param (1)),
           SQL_Select
             (F_References,
              From => Database.Entity_Refs & Database.Files & Files3
              & Database.Reference_Kinds,
              Where => Database.Entity_Refs.File = Database.Files.Id
              and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
              and Database.Reference_Kinds.Is_Real
              and Database.Files.Project = Files3.Id
              and not Database.Reference_Kinds.Is_Implicit
              and Database.Entity_Refs.Entity = Integer_Param (1)),
           Order_By => Database.Files.Path
           & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "references_no_implicit");

   Q_References_All_Kinds : constant Prepared_Statement :=
     Prepare
       (SQL_Union
          (SQL_Select
             (F_References_Decl,
              From => Database.Entities & Database.Files & Files3,
              Where => Database.Entities.Decl_File = Database.Files.Id
              and Database.Files.Project = Files3.Id
              and Database.Entities.Id = Integer_Param (1)),
           SQL_Select
             (F_References,
              From => Database.Entity_Refs & Database.Files & Files3
              & Database.Reference_Kinds,
              Where => Database.Entity_Refs.File = Database.Files.Id
              and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
              and Database.Files.Project = Files3.Id
              and Database.Entity_Refs.Entity = Integer_Param (1)),
           Order_By => Database.Files.Path
           & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "references_all_kinds");
      Q_Bodies_List : constant SQL_Field_List :=
     To_List
       ((Q_Ref_File_Id => +Database.Files.Id,
         Q_Ref_File    => +Database.Files.Path,
         Q_Ref_Line    => +Database.Entity_Refs.Line,
         Q_Ref_Col     => +Database.Entity_Refs.Column,
         Q_Ref_Kind    => +Database.Reference_Kinds.Display,
         Q_Ref_Caller  => +Database.Entity_Refs.Caller,
         Q_Ref_Entity  => +Database.Entity_Refs.Entity,
         Q_Ref_Kind_Id => +Database.Reference_Kinds.Id,
         Q_Ref_Is_End_Of_Scope =>
           +Database.Reference_Kinds.Is_End));

   Q_Bodies_Sub_Mangled : constant SQL_Query :=
     SQL_Select
       (Entities2.Id,
        From => Database.Entities & Entities2,
        Where => Database.Entities.Id = Integer_Param (1)
        and Database.Entities.Mangled_Name /= ""
        and Entities2.Id /= Database.Entities.Id
        and Entities2.Exported
        and Entities2.Mangled_Name =
          Database.Entities.Mangled_Name);

   Q_Bodies_Sub_Mangled_Prep : constant Prepared_Statement :=
     Prepare (Q_Bodies_Sub_Mangled);

   Q_Bodies_Simple : constant Prepared_Statement :=
     Prepare (SQL_Select
              (Q_Bodies_List,
                 From => Database.Entity_Refs & Database.Files
                 & Database.Reference_Kinds,
                 Where =>
                   Database.Entity_Refs.File = Database.Files.Id
                 and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                 and SQL_In (Database.Reference_Kinds.Id, "'b','c'")
                 and Database.Entity_Refs.Entity = Integer_Param (1),

                 Order_By => Database.Files.Path
                 & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
                 Distinct => True),
                 On_Server => False, Name => "bodies_simple");

   Q_Bodies_With_Mangled : constant Prepared_Statement :=
     Prepare
       (SQL_Select
          (Q_Bodies_List,
           From => Database.Entity_Refs & Database.Files
           & Database.Reference_Kinds,
           Where =>
             Database.Entity_Refs.File = Database.Files.Id
             and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
             and SQL_In (Database.Reference_Kinds.Id, "'b','c'")
             and (Database.Entity_Refs.Entity = Integer_Param (1)
               --  Search all matching exported entities
             or SQL_In
               (Database.Entity_Refs.Entity, Q_Bodies_Sub_Mangled)),

           Order_By => Database.Files.Path
           & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
           Distinct => True),
        On_Server => False, Name => "bodies_with_mangled");
   --  Return the bodies of the entity.
   --  This query is more complex than Q_References_And_Kind because it also
   --  looks at mangled name for find matches in other languages.

   type File_Db_Info is record
      Id   : Integer;  --  Id in the files table

      Export_Mangled_Name : Boolean;
      --  Whether we should by default use the entity name as the mangled
      --  name. This is true for C, systematically. We also set it true for C++
      --  (in case extern"C" was used), but it might get overridden later on.
   end record;

   package VFS_To_Ids is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => File_Db_Info,
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
      Element_Type    => File_Db_Info);
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
      Id         : Integer;  --  for the LI file
      LI         : GNATCOLL.Projects.Library_Info;
      Stamp      : Time;
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

   function Char_Value
     (R : Forward_Cursor; Field : Field_Index) return Character;
   --  Return a given part of the cursor as character

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
     (DB                  : Database_Connection;
      LI                  : LI_Info;
      Default_Iconv_State : Iconv_T;
      Source_To_Id        : in out VFS_To_Ids.Map;
      Project_To_Id       : VFS_To_Ids.Map;
      Entity_Decl_To_Id   : in out Loc_To_Ids.Map;
      Entity_Renamings    : in out Entity_Renaming_Lists.List;
      Visited_ALI_Units   : in out VFS_To_Ids.Map;
      Visited_GLI_Files   : in out VFS_To_Ids.Map);
   --  Parse the contents of a single LI file.
   --  VFS_To_Id is a local cache for the entries in the files table.
   --  Project is the one to which LI belongs.
   --
   --  Entity_Decl_To_Id maps a "file|line.col" to an entity id. This is filled
   --  during a first pass, and is needed to resolve references to parent
   --  types, index types,... during the second pass. This table does not
   --  include the name of the entity, since this is unknown when seeing the
   --  xref. But while parsing a given ALI file, the location is always unique
   --  (which would be potentially false if sharing this table for multiple
   --  ALIs)
   --
   --  Source_To_Id is a cache for source files. It is specific to each root
   --  aggregated project, since the same full path could belong to multiple
   --  projects, each in its own aggregated tree.
   --
   --  Project_To_Id maps full path to id in the files table for project files.
   --
   --  Visited_ALI_Units is a subset of VFS_To_Id, containing all the files
   --  from 'U' lines that have already been processed. In general, the files
   --  will occur only once since they have a single associated LI file.
   --  However, in the case of multi-unit source files in Ada, the file might
   --  already be in Visited_ALI_Units, in which case we need to do fewer
   --  cleanup prio to parsing the file.
   --
   --  Visited_GLI_Files is a subset of VFS_To_Id, containing C/C++ include
   --  files that have already been processed (ie. files from 'D' lines in
   --  GLI files).

   function Single_Entity_From_E2e
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      E2e    : Integer) return Entity_Information;
   --  Do a query on E2e, and returns the single matching entity (or No_Entity)

   procedure Init_For_Entity
     (Self   : in out Recursive_References_Cursor'Class;
      Entity : Entity_Information);
   procedure Init_For_Entity
     (Self   : in out Recursive_Entities_Cursor'Class;
      Entity : Entity_Information);
   --  Self will navigate all information extracted from Entity.

   procedure Get_Documentation_Before
     (Context       : Language_Syntax;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Allow_Blanks  : Boolean := False);
   procedure Get_Documentation_After
     (Context       : Language_Syntax;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural);
   --  Get the comment just before or just after Decl_Index, skipping code
   --  lines as needed.
   --  If Allow_Blanks is True, then skip blank lines before looking for
   --  comments.

   type Comment_Type is (No_Comment, Comment_Single_Line, Comment_Multi_Line);
   function Looking_At_Start_Of_Comment
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : Natural) return Comment_Type;
   --  Whether we have the start of a comment at Index in Buffer

   procedure Skip_To_Current_Comment_Block_Start
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : in out Natural);
   --  Assuming that Index is at the beginning or inside a comment line, moves
   --  upward in the file till the end of the current block of comments. This
   --  block is defined as a group of commented out lines, until a non-comment
   --  line is seen.
   --  If Index is not at the beginning or inside a comment line, Index is set
   --  to 0.

   procedure Skip_To_Current_Comment_Block_End
     (Context            : Language_Syntax;
      Buffer             : String;
      Index              : in out Natural;
      Ignore_Blank_Lines : Boolean := False);
   --  Same as Skip_To_Current_Comment_Block_Start, except we move forward to
   --  the beginning of the last line of comments in the block.
   --  If Ignore_Blank_Lines is set to True, blocks separated from one another
   --  with blank lines are considered as a single one.

   procedure Skip_To_Previous_Comment_Start
     (Context      : Language_Syntax;
      Buffer       : String;
      Index        : in out Natural;
      Allow_Blanks : Boolean := False);
   --  Skip lines of code (backward) until we find the start of a comment.
   --  If we see an empty line first Index is set to 0, unless Allow_Blanks.
   --  Likewise if no comment is found before the beginning of the buffer.

   procedure Skip_To_Next_Comment_Start
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : in out Natural);
   --  Skip lines of code until we find the beginning of a comment.
   --  If we see an empty line first Index is set to 0.
   --  Likewise if no comment is found before the end of the buffer.

   procedure Initialize_DB
     (Database        : in out Xref_Database;
      DB              : Database_Connection;
      From_DB_Name    : String;
      DB_Created      : out Boolean;
      Force           : Boolean);
   --  Initialize the database if needed (copy from disk or create db)
   --  Force should be True when initializing an in-memory database for the
   --  first time.

   procedure Search_LI_Files_To_Update
     (Database : Xref_Database;
      LI_Files : Library_Info_List;
      LIs      : in out LI_Lists.List;
      Force_Refresh : Boolean);
   --  Process the list of all LI files (Lib_Files) to detect those that
   --  need updating. This needs access to an existing database to check
   --  which files are up to date.
   --  If Force_Refresh is true, all LI files will be marked as needing
   --  update.

   ---------------------
   -- Create_Database --
   ---------------------

   procedure Create_Database
     (Connection      : access Database_Connection_Record'Class)
   is
      Start   : Time;
      Started : Boolean;
   begin
      if Active (Me_Timing) then
         Start := Clock;
      end if;

      Started := Connection.Start_Transaction;
      GNATCOLL.Xref.Database.Create_Database (Connection);

      Connection.Execute ("PRAGMA user_version=" & Schema_Version'Img & ";");

      if Started then
         Connection.Commit_Or_Rollback;
      end if;

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
      --

      if Lines.Max < High then
         Test_From := Low;

         if Low < Lines.Max then
            Lines.Lines (Lines.Max + 1 .. High) :=  --  force
              (others => (Entity => Entity, Scope => Scope));
            Test_To   := Lines.Max;
         else
            Lines.Lines (Lines.Max + 1 .. Low - 1) :=  -- reset
              (others => (Entity => -1, Scope => Integer'Last));
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
            Lines.Lines (Line) :=
              (Entity => Entity, Scope => Scope);
         end if;
      end loop;

--        for Line in Lines.Lines'First .. Lines.Max loop
--           Put (Line'Img & "(" & Lines.Lines (Line).Entity'Img & ")");
--        end loop;
--        New_Line;
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
      Line       : Integer) return Integer
   is
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
     (DB                  : Database_Connection;
      LI                  : LI_Info;
      Default_Iconv_State : Iconv_T;
      Source_To_Id        : in out VFS_To_Ids.Map;
      Project_To_Id       : VFS_To_Ids.Map;
      Entity_Decl_To_Id   : in out Loc_To_Ids.Map;
      Entity_Renamings    : in out Entity_Renaming_Lists.List;
      Visited_ALI_Units   : in out VFS_To_Ids.Map;
      Visited_GLI_Files   : in out VFS_To_Ids.Map)
   is
      M      : Mapped_File;
      Str    : Str_Access;
      Last   : Integer;
      Index  : Integer;

      LI_Project : constant Project_Type :=
        Project_Type (LI.LI.LI_Project.all);

      ALI_Id   : constant Integer := LI.Id;

      Start           : Integer;
      Current_Unit_Id : Integer := -1;
      Dep_Id          : Integer;

      D_Line_Id       : Positive := 1;
      --  Current "D" line index

      Depid_To_Id     : Depid_To_Ids.Vector;

      Is_GLI_File     : constant Boolean :=
                          LI.LI.Library_File.File_Extension = ".gli";

      Always_Parse_E2E : constant Boolean := Is_GLI_File;

      --  Whether the "entity-to-entity" reference should be systematically
      --  parsed. If False, this information is only parsed if the current X
      --  section relates to a source file which is a main unit for the LI
      --  file (i.e. has a U line). The latter is more efficient, but in C and
      --  C++ cannot be used because there is no ".gli" for .h files only.

      Unit_Files : Depid_To_Ids.Vector;
      --  Contains the list of units associated with the current ALI (these
      --  are the ids in the "files" table). This list generally only contains
      --  a few elements, so is reasonably fast.
      --  These are the files from the "U" lines (spec, body and separates).

      Scope_Trees      : Scope_Tree_Array_Access;
      Decl_Scope_Trees : Scope_Tree_Array_Access;
      --  There is a special case for the first line of an entity's scope:
      --  the goal is that subprograms belong to their enclosing package, not
      --  to themselves, but their parameters belong to them. For instance:
      --       package body P is
      --          procedure Proc (A : Integer) is
      --  The caller at declaration for Proc is "P", but for A is "Proc", so
      --  we need two pieces of information for the scope for a single line
      --  of code. We do this by creating two scope trees, one of which
      --  skips the first line of a scope, the other doesn't.

      Current_X_File : Integer;
      Current_X_File_Export_Mangled : Boolean;
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

      Iconv_State : Iconv_T := Default_Iconv_State;
      --  Iconv state to be used to convert names in ALI file.

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

      procedure Set_Mangled_Name (Mangled : String; Exported : Boolean);
      --  Set the mangled name for the entity, and whether this is an export or
      --  an import.

      function Get_Natural return Natural;
      pragma Inline (Get_Natural);
      --  Read an integer at the current position, and moves Index after it.

      function Get_Char return Character;
      pragma Inline (Get_Char);
      --  Return the current character, and move forward

      function Get_Or_Create_Entity
        (Decl_File        : Integer;
         Decl_Line        : Integer;
         Decl_Column      : Integer;
         Name             : String;
         Kind             : Character;
         Set_Mangled_Name : Boolean;
         Is_Global        : Boolean;
         Is_Static_Local  : Boolean) return Integer;
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
        (Basename    : Filesystem_String;
         Project     : Project_Type;
         Is_ALI_Unit : Boolean := False) return File_Db_Info;
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
            if Element (C).Id = Id then
               if Active (Me_Debug) then
                  Trace (Me_Debug, "Is_Unit_File Id=" & Id'Img
                         & " Index=" & Index'Img);
               end if;

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
            Xref_File := Depid_To_Id.Element (Xref_Line).Id;
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
                               & (+LI.LI.Library_File.Unix_Style_Full_Name
                                 (Normalize => True))
                               & " at index" & Index'Img);
                     end if;

                     Ref_Entity := DB.Insert_And_Get_PK
                       (Query_Insert_Entity,
                        Params =>
                          (1 => +Name'Unrestricted_Access,
                           2 => +'I',
                           3 => +(-1),
                           4 => +(-1),
                           5 => +(-1),
                           6 => +True,    --  is_global
                           7 => +False),  --  is_static_local
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
              and then (Current_X_File_Unit_File_Index /= -1
                        or else Always_Parse_E2E)
              and then Xref_File /= -1
            then
               Ref_Entity := Get_Or_Create_Entity
                 (Decl_File   => Xref_File,
                  Decl_Line   => Xref_Line,
                  Decl_Column => Xref_Col,
                  Name        => "",
                  Kind        => Xref_Kind,
                  Is_Global   => False,     --  unknown
                  Is_Static_Local => False, --  unknown
                  Set_Mangled_Name => False);
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
                          4 => +E2e_Order));
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

            --  Ignore language info
            for Pos in Start .. Index - 1 loop
               if Str (Pos) = ',' then
                  Start := Pos + 1;
                  exit;
               end if;
            end loop;

            declare
               Name : constant String :=
                 String (Str (Start .. Index - 2));
            begin
               case Xref_Kind is
                  when 'i' =>
                     Set_Mangled_Name (Name, Exported => True);
                  when 'b' =>
                     Set_Mangled_Name (Name, Exported => False);
                  when others =>
                     Trace (Me_Error, "Error: unexpected entity kind"
                            & " before a mangled name info: '"
                            & Xref_Kind'Img & "'");
               end case;
            end;
         end if;
      end Skip_Import_Info;

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces is
      begin
         while Str (Index) = ' '
           or else Str (Index) = ASCII.HT
         loop
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
           and then Str (Index) /= ASCII.CR
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
         if Str (Index) = '"' then
            --  Operators are quoted

            Index := Index + 1;

            while Str (Index) /= '"' loop
               Index := Index + 1;
            end loop;
            Index := Index + 1;   --  skip closing quote

         --  C++ operators are represented as:
         --    93V7*operator new 93r12
         --    93V7*operator=={bool}
         --    95V7*operator new [] 95i<cpp,_Znam>7 1|88s34
         --    840V7*operator[]{120J61} 1|46s26
         --  so we need to allow a space in the name in some cases.

         elsif Index + 8 <= Str'Last
           and then Str (Index) = 'o'
           and then Str (Index + 1) = 'p'
           and then Str (Index + 2) = 'e'
           and then Str (Index + 3) = 'r'
           and then Str (Index + 4) = 'a'
           and then Str (Index + 5) = 't'
           and then Str (Index + 6) = 'o'
           and then Str (Index + 7) = 'r'
         then
            Index := Index + 8;

            case Str (Index) is
               when '=' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  == operator
                  else
                     Index := Index + 1;  --  = operator
                  end if;

               when '<' =>
                  if Index < Str'Last and then Str (Index + 1) = '<' then
                     if Index + 1 < Str'Last
                        and then Str (Index + 2) = '='
                     then
                        Index := Index + 3;  --  <<= operator
                     else
                        Index := Index + 2;   --  << operator
                     end if;
                  elsif Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;   --  <= operator
                  else
                     Index := Index + 1;   --  < operator
                  end if;

               when '>' =>
                  if Index < Str'Last and then Str (Index + 1) = '>' then
                     if Index + 1 < Str'Last
                        and then Str (Index + 2) = '='
                     then
                        Index := Index + 3;  --  >>= operator
                     else
                        Index := Index + 2;   --  >> operator
                     end if;
                  elsif Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  >= operator
                  else
                     Index := Index + 1;  --  > operator
                  end if;

               when '-' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  -= operator
                  elsif Index < Str'Last and then Str (Index + 1) = '-' then
                     Index := Index + 2;  --  -- operator
                  elsif Index + 2 <= Str'Last
                     and then Str (Index + 1 .. Index + 2) = ">*"
                  then
                     Index := Index + 3;   --  ->*() operator (smart pointers)
                  elsif Index < Str'Last and then Str (Index + 1) = '>' then
                     Index := Index + 2;   --  ->() operator (smart pointers)
                  else
                     Index := Index + 1;  --  - operator
                  end if;

               when '+' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  += operator
                  elsif Index < Str'Last and then Str (Index + 1) = '+' then
                     Index := Index + 2;  --  ++ operator
                  else
                     Index := Index + 1;  --  + operator
                  end if;

               when '*' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  *= operator
                  elsif Index + 2 < Str'Last
                     and then Str (Index + 1 .. Index + 2) = "()"
                  then
                     Index := Index + 3;   --  *() operator (smart pointers)
                  else
                     Index := Index + 1;  --  * operator
                  end if;

               when '/' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  /= operator
                  else
                     Index := Index + 1;  --  / operator
                  end if;

               when '!' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;  --  != operator
                  else
                     Index := Index + 1;  --  ! operator (not)
                  end if;

               when '[' =>
                  if Index < Str'Last and then Str (Index + 1) = ']' then
                     Index := Index + 2;  --  [] operator
                  else
                     Index := Index + 1;  --  ???
                  end if;

               when '(' =>
                  if Index < Str'Last and then Str (Index + 1) = ')' then
                     Index := Index + 2;  --  () operator
                  else
                     Index := Index + 1;  --  ???
                  end if;

               when '%' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;   --  %= operator
                  else
                     Index := Index + 1;   --  % operator
                  end if;

               when '|' =>
                  if Index < Str'Last and then Str (Index + 1) = '|' then
                     Index := Index + 2;   --  || operator
                  elsif Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;   --  |= operator
                  else
                     Index := Index + 1;   --  | operator
                  end if;

               when '&' =>
                  if Index < Str'Last and then Str (Index + 1) = '&' then
                     Index := Index + 2;   --  && operator
                  elsif Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;   --  &= operator
                  elsif Index < Str'Last and then Str (Index + 1) = '(' then
                     Index := Index + 4;   --  &() operator (smart pointers)
                  else
                     Index := Index + 1;   --  & operator
                  end if;

               when '^' =>
                  if Index < Str'Last and then Str (Index + 1) = '=' then
                     Index := Index + 2;   --  ^= operator
                  else
                     Index := Index + 1;   --  ^ operator
                  end if;

               when ' ' =>
                  if Starts_With
                    (String (Str (Index + 1 .. Str'Last)), "new")
                  then
                     Index := Index + 4;  --  new operator

                     if Str (Index) = ' '
                       and then Str (Index + 1) = '['
                       and then Str (Index + 2) = ']'
                     then
                        Index := Index + 3;  --  new[] operator
                     end if;

                  elsif Starts_With
                    (String (Str (Index + 1 .. Str'Last)), "delete")
                  then
                     Index := Index + 7;  --  delete operator

                     if Str (Index) = ' '
                       and then Str (Index + 1) = '['
                       and then Str (Index + 2) = ']'
                     then
                        Index := Index + 3;  --  delete[] operator
                     end if;

                  else
                     --  type conversion operators (int(), float(),...)
                     while Index <= Str'Last and then Str (Index) /= '{' loop
                        Index := Index + 1;
                     end loop;
                  end if;

               when '~' | ',' =>
                  Index := Index + 1;

               when others =>
                  --  unexpected
                  Index := Index + 1;
            end case;

         else
            Index := Index + 1;

            --  Entity names can contain extra information, like
            --  pointed type,... So we need to extract the name
            --  itself and will store the extra information in a
            --  second step

            while Str (Index) /= ASCII.LF
              and then Str (Index) /= ASCII.CR
              and then Str (Index) /= '{'
              and then Str (Index) /= '<'
              and then Str (Index) /= '('
              and then Str (Index) /= ' '
              and then Str (Index) /= '='
              and then Str (Index) /= '['
            loop
               Index := Index + 1;
            end loop;
         end if;
      end Skip_To_Name_End;

      ------------------------
      -- Insert_Source_File --
      ------------------------

      function Insert_Source_File
        (Basename        : Filesystem_String;
         Project         : Project_Type;
         Is_ALI_Unit     : Boolean := False) return File_Db_Info
      is
         Info : constant File_Info :=
           Project.Create_From_Project (Basename);
         Found  : VFS_To_Ids.Cursor;
         Result : File_Db_Info;
      begin
         if Info.File = GNATCOLL.VFS.No_File then
            if Active (Me_Error) then
               Trace (Me_Error, "File " & (+Basename)
                      & " not found in project "
                      & Project.Project_Path.Display_Full_Name);
            end if;
            return (Id                  => -1,
                    Export_Mangled_Name => False);
         end if;

         Found := Source_To_Id.Find (Info.File);
         if Has_Element (Found) then
            Result := Element (Found);
         else
            declare
               Name  : aliased String :=
                 +Info.File.Unix_Style_Full_Name (Normalize => True);
               Files : Forward_Cursor;
               Id    : Integer;  --  for the project
            begin
               if Info.Project = No_Project then
                  Id := No_Project_Id;
               else
                  Id := Project_To_Id.Element (Info.Project.Project_Path).Id;
               end if;

               Files.Fetch
                 (DB, Query_Get_File_And_Project,
                  Params => (1 => +Name'Unchecked_Access,
                             2 => +Id));

               if Files.Has_Row then
                  Result := (Id   => Files.Integer_Value (0),
                             Export_Mangled_Name =>
                               Files.Value (2) = "c"
                               or else Files.Value (2) = "c++");
               else
                  declare
                     Lang : aliased String := Info.Language;
                  begin
                     Result :=
                       (Id => DB.Insert_And_Get_PK
                           (Query_Insert_Source_File,
                            Params => (1 => +Name'Unchecked_Access,
                                       2 => +Lang'Unrestricted_Access,
                                       3 => +Id),
                            PK => Database.Files.Id),
                        Export_Mangled_Name =>
                          Lang = "c" or else Lang = "c++");
                  end;
               end if;

               Source_To_Id.Insert (Info.File, Result);
            end;
         end if;

         if Is_ALI_Unit then
            if Active (Me_Debug) then
               Trace (Me_Debug, "Append to Unit_Files "
                      & Result.Id'Img);
            end if;
            Unit_Files.Append (Result);
            Grow_As_Needed (Scope_Trees, Integer (Unit_Files.Length));
            Grow_As_Needed (Decl_Scope_Trees, Integer (Unit_Files.Length));

            --  Clear previous info known for this source file.
            --  This cannot be done with a single query when we
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

            if not Visited_ALI_Units.Contains (Info.File) then
               Visited_ALI_Units.Include (Info.File, Result);
            end if;
         end if;

         return Result;
      end Insert_Source_File;

      ----------------------
      -- Set_Mangled_Name --
      ----------------------

      procedure Set_Mangled_Name (Mangled : String; Exported : Boolean) is
      begin
         DB.Execute
           (Query_Set_Entity_Mangled_Name,
            Params => (1 => +Current_Entity,
                       2 => +Mangled'Unrestricted_Access,
                       3 => +Exported));
      end Set_Mangled_Name;

      --------------------------
      -- Get_Or_Create_Entity --
      --------------------------

      function Get_Or_Create_Entity
        (Decl_File        : Integer;
         Decl_Line        : Integer;
         Decl_Column      : Integer;
         Name             : String;
         Kind             : Character;
         Set_Mangled_Name : Boolean;
         Is_Global        : Boolean;
         Is_Static_Local  : Boolean) return Integer
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

         N : aliased String := GNATCOLL.Iconv.Iconv
           (Iconv_State, Name, Ignore_Errors => True);
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

            if N'Length /= 0 then
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

               if Set_Mangled_Name then
                  Candidate := DB.Insert_And_Get_PK
                    (Query_Insert_Entity_With_Mangled,
                     Params =>
                       (1 => +N'Unrestricted_Access,   --  empty string
                        2 => +'P',  --  unknown
                        3 => +Decl_File,
                        4 => +Decl_Line,
                        5 => +(-1),
                        6 => +N'Unrestricted_Access,
                        7 => +Is_Global,
                        8 => +Is_Static_Local),
                     PK => Database.Entities.Id);
               else
                  Candidate := DB.Insert_And_Get_PK
                    (Query_Insert_Entity,
                     Params =>
                       (1 => +N'Unrestricted_Access,   --  empty string
                        2 => +'P',  --  unknown
                        3 => +Decl_File,
                        4 => +Decl_Line,
                        5 => +(-1),
                        6 => +Is_Global,
                        7 => +Is_Static_Local),
                     PK => Database.Entities.Id);
               end if;
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
              or else N'Length = 0  --  Or do we still have forward decl
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

         if N'Length /= 0
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
               if N'Length /= 0 and then R.Value (1) = N then
                  Candidate := R.Integer_Value (0);
                  Candidate_Is_Forward := False;
                  exit;
               elsif N'Length = 0 and then R.Value (1) /= "" then
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
                  --  that's the good one. Since the entity still exists, we
                  --  will not want to remove it as obsolete once we are done
                  --  parsing, so that other LI files referencing it still have
                  --  that info.

                  DB.Execute
                    ("DELETE FROM temp_entities WHERE id="
                     & Candidate'Img & ";");

                  Entity_Decl_To_Id.Include
                    (Decl,
                     Entity_Info'(Id         => Candidate,
                                  Known_Name => True));

               elsif N'Length /= 0 then
                  --  We had a forward declaration in the database, we can
                  --  now update its name.
                  DB.Execute
                    (Query_Set_Entity_Name_And_Kind,
                     Params => (1 => +Candidate,
                                2 => +N'Unrestricted_Access,
                                3 => +Kind,
                                4 => +Is_Global,
                                5 => +Is_Static_Local));

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
            if Set_Mangled_Name then
               Candidate := DB.Insert_And_Get_PK
                 (Query_Insert_Entity_With_Mangled,
                  Params =>
                    (1 => +N'Unrestricted_Access,
                     2 => +Kind,
                     3 => +Decl_File,
                     4 => +Decl_Line,
                     5 => +Decl_Column,
                     6 => +N'Unrestricted_Access,
                     7 => +Is_Global,
                     8 => +Is_Static_Local),
                  PK => Database.Entities.Id);
            else
               Candidate := DB.Insert_And_Get_PK
                 (Query_Insert_Entity,
                  Params =>
                    (1 => +N'Unrestricted_Access,
                     2 => +Kind,
                     3 => +Decl_File,
                     4 => +Decl_Line,
                     5 => +Decl_Column,
                     6 => +Is_Global,
                     7 => +Is_Static_Local),
                  PK => Database.Entities.Id);
            end if;

            Entity_Decl_To_Id.Insert
              (Decl,
               Entity_Info'(Id         => Candidate,
                            Known_Name => N'Length /= 0));
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
               declare
                  Info : constant File_Db_Info :=
                    Depid_To_Id.Element (Get_Natural);
               begin
                  Current_X_File := Info.Id;
                  Current_X_File_Export_Mangled := Info.Export_Mangled_Name;
               end;

               Current_X_File_Unit_File_Index := Is_Unit_File (Current_X_File);
               if Active (Me_Debug) then
                  Trace (Me_Debug, "Process_Xref_Section current="
                         & Current_X_File'Img & " is_unit_file="
                         & Current_X_File_Unit_File_Index'Img);
               end if;

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
         Is_Static_Local  : Boolean;
         Ref_Entity : Integer;
         Name_End, Name_Start : Integer;
         Entity_Kind : Character;
         Visibility : Character;
         Eid : E2e_Id;
         Order : Natural := 0;
         Will_Insert_Ref : Boolean;
         Instance : Unbounded_String;
         End_Of_Spec_Line : Natural := 0;

      begin
         if Str (Index) = '.' then
            --  Same entity as before, so we do not change current entity
            Index := Index + 2;  --  First ref on that line

         else
            Get_Ref;
            Entity_Kind      := Xref_Kind;

            Visibility       := Get_Char;
            Is_Library_Level := Visibility = '*';
            Is_Static_Local  := Visibility = '+';
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
                     Caller : Integer :=
                       Get_Caller (Scope_Trees,
                                   Current_X_File_Unit_File_Index,
                                   Xref_Line);
                  begin
                     if Caller /= -1 then
                        if Caller = Current_Entity then
                           Caller := Get_Caller
                             (Decl_Scope_Trees,
                              Current_X_File_Unit_File_Index,
                              Xref_Line);
                        end if;

                        if Caller /= Current_Entity then
                           DB.Execute
                             (Query_Set_Caller_At_Decl,
                              Params => (1 => +Current_Entity,
                                         2 => +Caller));
                        end if;
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
                  Kind        => Xref_Kind,
                  Is_Global   => Is_Library_Level,
                  Is_Static_Local => Is_Static_Local,
                  Set_Mangled_Name => Current_X_File_Export_Mangled);
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

                     if Xref_Kind = 'o' then
                        Eid := E2e_Overrides;
                     else
                        case Entity_Kind is
                        when 'A' | 'a' =>
                           Eid := E2e_Has_Index;
                        when 'G' | 'u' | 'U' | 'v' | 'V' | 'y' | 'x' =>
                           Eid := E2e_Overrides;
                        when 'c' =>  --  Temporary workaround for M220-019
                           Eid := E2e_Of_Type;
                        when others =>
                           Eid := E2e_Parent_Type;
                        end case;
                     end if;

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
                           Eid := E2e_Of_Type;  --  return type
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
                           Eid := E2e_Of_Type;  --  Return type
                        when 'n' =>
                           Eid := E2e_From_Enumeration;
                        when 'J' | 'R' =>
                           Eid := E2e_Parent_Type;
                        when others =>
                           Eid := E2e_Of_Type;
                     end case;

                     if not Get_Ref_Or_Predefined
                       (Endchar => '}', Eid => Eid, E2e_Order => Order,
                        Process_E2E => Process_E2E)
                     then
                        return;
                     end if;

                  when ' ' | ASCII.CR =>
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
           and then Str (Index) /= ASCII.CR
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
                  End_Of_Spec_Line := Xref_Line;
               when '<' =>
                  Eid := E2e_Out_Parameter;
                  End_Of_Spec_Line := Xref_Line;
               when '=' =>
                  Eid := E2e_In_Out_Parameter;
                  End_Of_Spec_Line := Xref_Line;
               when '^' =>
                  Eid := E2e_Access_Parameter;
                  End_Of_Spec_Line := Xref_Line;
               when 'p' | 'P' =>
                  Eid := E2e_Has_Primitive;
               when 'k' =>
                  Eid := E2e_Parent_Package;
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
                  if Xref_File_Unit_File_Index /= -1 then
                     End_Of_Spec_Line := Xref_Line;
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
                     if Body_Start_Line + 1 <= Xref_Line then
                        Insert (Decl_Scope_Trees (Xref_File_Unit_File_Index),
                                Entity => Current_Entity,
                                Low    => Body_Start_Line + 1,
                                High   => Xref_Line);
                     end if;
                  end if;

               when others =>
                  null;
                  --  real references, or not an entity->entity relationship
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
                     Caller : Integer :=
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
                        if Caller = Current_Entity then
                           Caller := Get_Caller
                             (Decl_Scope_Trees, Xref_File_Unit_File_Index,
                              Xref_Line);
                        end if;

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
                 or else (Current_X_File_Unit_File_Index /= -1
                          or else Always_Parse_E2E);

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
                                   4 => +Order));
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

         --  Not all subprogram specs get a 'e' line, so we also try to guess
         --  the end of spec by looking at the last parameter declaration.

         if Process_Scopes
           and then Current_X_File_Unit_File_Index /= -1
           and then End_Of_Spec_Line /= 0
         then
            Insert (Scope_Trees (Current_X_File_Unit_File_Index),
                    Entity => Current_Entity,
                    Low    => Spec_Start_Line,
                    High   => End_Of_Spec_Line);
            if Spec_Start_Line + 1 <= Xref_Line then
               Insert (Decl_Scope_Trees (Current_X_File_Unit_File_Index),
                       Entity => Current_Entity,
                       Low    => Spec_Start_Line + 1,
                       High   => End_Of_Spec_Line);
            end if;
         end if;

         if Index <= Str'Last and then Str (Index) = ASCII.CR then
            if Index = Str'Last or else Str (Index + 1) = ASCII.LF then
               Index := Index + 1;
            end if;
         end if;
      end Process_Entity_Line;

      Start_Of_X_Section : Integer;

   begin  --  Parse_LI
      if Active (Me_Parsing) then
         Increase_Indent
           (Me_Parsing, "Parse LI "
            & (+LI.LI.Library_File.Unix_Style_Full_Name
              (Normalize => True)) & " in project "
            & LI.LI.LI_Project.Project_Path.Display_Full_Name);
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
            if Active (Me_Parsing) then
               Decrease_Indent (Me_Parsing);
            end if;
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

               --  In general, we need to remove old references already known
               --  in this source file. However, in the case of source files
               --  with multiple units, we should only do so for the first ALI
               --  file we are seeing, otherwise later ALI files (which have
               --  also changed, necessarily) will remove the references we
               --  just added.

               Current_Unit_Id := Insert_Source_File
                 (Basename     => Filesystem_String (Str (Start .. Index - 1)),
                  Project      => LI_Project,  --  same as the LI
                  Is_ALI_Unit  => True).Id;
               if Current_Unit_Id /= -1 then
                  DB.Execute
                    (Query_Set_ALI,
                     Params => (1 => +Current_Unit_Id,
                                2 => +ALI_Id));
               end if;

            when 'W' | 'Z' =>
               --  Describes a "with" dependency with the last seen U line.
               --  There are two cases:
               --      W ada.text_io%s  a-textio.adb   system.ali
               --      W unchecked_deallocation%s
               --  The second line does not have ALI information.
               --  On the first line, the file name is that of the body unless
               --  there is only a spec, and yet the dependency is in general
               --  on the spec.

               Index := Index + 2;
               Dep_Id := -1;

               declare
                  Word_Start : constant Integer := Index;
                  Part : Unit_Parts;
               begin
                  Skip_Word;

                  if Str (Index - 1) = 's' then
                     Part := Unit_Spec;
                  else
                     Part := Unit_Body;
                  end if;

                  declare
                     F : constant Filesystem_String :=
                       LI_Project.File_From_Unit
                         (Unit_Name => String (Str (Word_Start .. Index - 3)),
                          Part      => Part,
                          Language  => "ada");
                  begin
                     if F /= "" then
                        Skip_Spaces;
                        Skip_Word;
                        Dep_Id := Insert_Source_File (F, LI_Project).Id;

                     elsif Str (Index) /= ASCII.LF
                       and then Str (Index) /= ASCII.CR
                     then
                        Skip_Spaces;
                        Start := Index;
                        Skip_Word;
                        Dep_Id := Insert_Source_File
                          (Filesystem_String (Str (Start .. Index - 1)),
                           LI_Project).Id;
                     else
                        --  second format ("unchecked_deallocation%s\n").
                        Dep_Id := -1;
                     end if;

                     if Current_Unit_Id /= -1
                       and then Dep_Id /= -1
                     then
                        DB.Execute
                          (Query_Set_File_Dep,
                           Params =>
                             (1 => +Current_Unit_Id, 2 => +Dep_Id));
                     end if;
                  end;
               end;

            when 'D' =>
               --  All dependencies for all units (used as indexes in xref)

               Index := Index + 2;
               Start := Index;
               Skip_Word;

               declare
                  Base_Last : constant Integer := Index - 1;
                  Basename  : constant Filesystem_String :=
                                Filesystem_String (Str (Start .. Base_Last));
                  Info : File_Db_Info;
                  File : File_Info;

               begin
                  Skip_Spaces;
                  Skip_Word;
                  Skip_Spaces;
                  Skip_Word;

                  if not Is_GLI_File then

                     --  Is this a dependency for a separate unit ? GNAT will
                     --  not generate a 'U' line for separates, but special D
                     --  lines that list the name of the unit, as in:
                     --       D a~bar.adb 20111220095636 1986a86b a.bar

                     Info := Insert_Source_File
                       (Filesystem_String (Str (Start .. Base_Last)),
                        LI_Project,
                        Is_ALI_Unit =>
                          Str (Index) /= ASCII.LF
                          and then Str (Index) /= ASCII.CR);

                  --  Handle C/C++ include files

                  else
                     File := LI_Project.Create_From_Project (Basename);
                     if File.File = GNATCOLL.VFS.No_File then
                        if Active (Me_Parsing) then
                           Trace (Me_Parsing, "On D line, file " & (+Basename)
                                  & " not found in "
                                  & LI_Project.Project_Path.Display_Full_Name);
                        end if;
                     end if;

                     --  The first time we visit a dependency of an include
                     --  file we handle it as if it were a true separate ALI
                     --  unit (to compute its scope-tree).

                     if not Visited_ALI_Units.Contains (File.File)
                       and then not Visited_GLI_Files.Contains (File.File)
                     then
                        Info := Insert_Source_File
                          (File.File.Full_Name.all,
                           LI_Project, Is_ALI_Unit => True);
                        Visited_GLI_Files.Include (File.File, Info);
                     else
                        Info := Insert_Source_File
                          (File.File.Full_Name.all,
                           LI_Project, Is_ALI_Unit => False);
                     end if;
                  end if;

                  Dep_Id := Info.Id;

                  Depid_To_Id.Set_Length
                    (Ada.Containers.Count_Type (D_Line_Id));
                  Depid_To_Id.Replace_Element
                    (Index    => D_Line_Id,
                     New_Item => Info);
               end;

               D_Line_Id := D_Line_Id + 1;

            when 'A' =>
               --  Compiler switches.

               Skip_Word;
               Skip_Spaces;
               Start := Index;
               Skip_Word;

               if Str (Start .. Index - 1) = "-gnatW8" then
                  --  File is marked as UTF-8 encoded

                  Iconv_State := Iconv_Open
                    (To_Code => UTF8, From_Code => UTF8, Ignore => True);
               end if;

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
      Free (Decl_Scope_Trees);
      Close (M);

      --  Close Iconv state when it was opened by this function.

      if Iconv_State /= Default_Iconv_State then
         Iconv_Close (Iconv_State);
      end if;

      if Active (Me_Parsing) then
         Decrease_Indent (Me_Parsing);
      end if;
   end Parse_LI;

   -------------------
   -- Initialize_DB --
   -------------------

   procedure Initialize_DB
     (Database        : in out Xref_Database;
      DB              : Database_Connection;
      From_DB_Name    : String;
      DB_Created      : out Boolean;
      Force           : Boolean)
   is
      Start : Time;
      R : Forward_Cursor;
      Is_Sqlite : constant Boolean := SQL.Sqlite.Is_Sqlite (Database.DB);
      Need_To_Create : Boolean;
   begin
      DB_Created := False;
      if Is_Sqlite and then (Force or else not Database.DB_Created) then
         Database.DB_Created := True;

         declare
            Current_DB : constant String := SQL.Sqlite.DB_Name (DB);
            Schema_Is_Valid : Boolean := True;
         begin
            R.Fetch (DB, "PRAGMA user_version;");
            if R.Integer_Value (0) /= Schema_Version then
               Schema_Is_Valid := False;
            end if;

            --  schema was never created, or is incorrect version

            if not Schema_Is_Valid then
               Need_To_Create := True;

               if Current_DB /= From_DB_Name
                 and then From_DB_Name /= ""
                 and then GNAT.OS_Lib.Is_Regular_File (From_DB_Name)
               then
                  Trace (Me_Debug, "Copying database from " & From_DB_Name);

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

                  R.Fetch (DB, "PRAGMA user_version;");
                  if R.Integer_Value (0) /= Schema_Version then
                     Trace (Me_Error,
                            "Schema Version from copied db was incorrect");
                  else
                     --  Schema version is good, no need to recreate
                     Need_To_Create := False;
                  end if;
               end if;

               if Need_To_Create then
                  Trace (Me_Timing, "Creating the database schema");
                  Create_Database (DB);
                  DB_Created := True;
               end if;
            end if;
         end;
      end if;
   end Initialize_DB;

   -------------------------------
   -- Search_LI_Files_To_Update --
   -------------------------------

   procedure Search_LI_Files_To_Update
     (Database : Xref_Database;
      LI_Files : Library_Info_List;
      LIs      : in out LI_Lists.List;
      Force_Refresh : Boolean)
   is
      Lib_Info : Library_Info_Lists.Cursor := LI_Files.First;
      Files    : Forward_Cursor;
      LI       : LI_Info;
   begin
      while Has_Element (Lib_Info) loop
         --  ??? Computing the timestamp is potentially slow
         --  on Windows (conversion to Time).

         LI :=
           (LI         => Element (Lib_Info),
            Id         => -1,   --  unknown in the database
            Stamp      => Element (Lib_Info).Library_File.File_Time_Stamp);

         declare
            N : aliased String :=
              +LI.LI.Library_File.Unix_Style_Full_Name (Normalize => True);
         begin
            Files.Fetch
              (Database.DB, Query_Get_File,
               Params => (1 => +N'Unchecked_Access));

            if Files.Has_Row then
               if not Force_Refresh
                 and then Files.Time_Value (1) = LI.Stamp
               then
                  LI.Id := -2;   --  Already up-to-date
               else
                  LI.Id := Files.Integer_Value (0);
                  Database.DB.Execute
                    (Query_Update_LI_File,
                     Params => (1 => +LI.Id, 2 => +LI.Stamp));
                  LIs.Append (LI);
               end if;

            else
               --  We need to insert the LI in the database now, because the
               --  same LI file could be seen again if we are using aggregate
               --  projects

               LI.Id := Database.DB.Insert_And_Get_PK
                 (Query_Insert_LI_File,
                  Params => (1 => +N'Unchecked_Access,
                             2 => +LI.Stamp),
                  PK => GNATCOLL.Xref.Database.Files.Id);
               LIs.Append (LI);
            end if;
         end;

         Next (Lib_Info);
      end loop;
   end Search_LI_Files_To_Update;

   ------------------------
   -- Parse_All_LI_Files --
   ------------------------

   procedure Parse_All_LI_Files
     (Self                : in out Xref_Database;
      Tree                : Project_Tree;
      Project             : Project_Type;
      Parse_Runtime_Files : Boolean := True;
      Show_Progress       : access procedure (Cur, Total : Integer) := null;
      From_DB_Name        : String := "";
      To_DB_Name          : String := "";
      ALI_Encoding        : String := GNATCOLL.Iconv.Locale;
      Force_Refresh       : Boolean := False)
   is
      Is_Sqlite       : constant Boolean := SQL.Sqlite.Is_Sqlite (Self.DB);

      Destroy_Indexes : Boolean := False;
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

      LI_Files          : Library_Info_List;
      LIs               : LI_Lists.List;

      Source_To_Id      : VFS_To_Ids.Map;
      Project_To_Id     : VFS_To_Ids.Map;

      Visited_ALI_Units : VFS_To_Ids.Map;
      Visited_GLI_Files : VFS_To_Ids.Map;
      Entity_Decl_To_Id : Loc_To_Ids.Map;
      Entity_Renamings  : Entity_Renaming_Lists.List;

      procedure Resolve_Renamings (DB : Database_Connection);
      --  The last pass in parsing a ALI file is to resolve all renamings, now
      --  that we can convert a reference to an entity

      procedure Backup_DB_If_Needed
        (DB : Database_Connection; To_DB : String);
      --  Dump the database to disk if needed.

      procedure Finalize_DB (DB : Database_Connection);
      --  After parsing LI files, do the last post-processings in the database
      --  (recreate indexes, analyze,...)

      procedure Start_Transaction
        (DB : Database_Connection;
         Destroy_Indexes : Boolean);
      --  Preparate the database for editing, once we have detected some LI
      --  files need to be updated.

      procedure Create_Project_Entries (DB : Database_Connection);
      --  Make sure that all project files have an entry in the files table

      procedure Parse_Files (DB : Database_Connection);
      --  Parse files that need it

      -----------------------
      -- Resolve_Renamings --
      -----------------------

      procedure Resolve_Renamings (DB : Database_Connection) is
         C     : Entity_Renaming_Lists.Cursor := Entity_Renamings.First;
         Ren   : Entity_Renaming;
         Start : Time;
      begin
         if Active (Me_Timing) then
            Start := Clock;
         end if;

         while Has_Element (C)
           and then Self.DB.Success
         loop
            Ren := Element (C);
            DB.Execute
              (Query_Set_Entity_Renames,
               Params => (1 => +Ren.Entity,
                          2 => +Ren.File,
                          3 => +Ren.Line,
                          4 => +Ren.Column,
                          5 => +Ren.Kind));
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
            DB.Execute
              ("CREATE INDEX entity_refs_loc on entity_refs(line, column)");

            --  These two indexes are slow to create, but without them the
            --  queries on e2e are too slow.

            DB.Execute ("CREATE INDEX e2e_from on e2e(fromEntity)");
            DB.Execute ("CREATE INDEX e2e_to on e2e(toEntity)");

            DB.Execute
              ("CREATE INDEX entity_decl_caller on entities(decl_caller)");
            DB.Execute
              ("CREATE INDEX refs_caller on entity_refs(caller)");

            if Active (Me_Timing) then
               Trace (Me_Timing,
                      "CREATE INDEXes: "
                      & Duration'Image (Clock - Start) & " s");
            end if;
         end if;

         --  Do this once we have restored the indexes, to speed up the search

         Resolve_Renamings (DB);

         --  Need to commit before we can change the pragmas

         DB.Commit_Or_Rollback;

         if DB.Has_Pragmas then
            DB.Execute ("PRAGMA foreign_keys=ON;");

            --  The default would be FULL, but we do not need to guard against
            --  system crashes in this application.

            DB.Execute ("PRAGMA synchronous=NORMAL;");

            --  The default would be DELETE, but we do not care enough about
            --  data integrity. WAL apparently allows readers even while there
            --  is a writer. MEMORY might corrupt the database if the writer is
            --  killed while processing.

            if Active (Me_Use_WAL) then
               DB.Execute ("PRAGMA journal_mode=WAL;");
            end if;

            --  We can store temporary tables in memory

            DB.Execute ("PRAGMA temp_store=MEMORY;");
         end if;

         --  Gather statistics to speed up the query optimizer. This isn't
         --  systematically needed, and might take a while to generate, so we
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

      procedure Start_Transaction
        (DB : Database_Connection;
         Destroy_Indexes : Boolean) is
      begin
         if DB.Has_Pragmas then

            --  Disable checks for foreign keys. This saves a bit of time when
            --  inserting the new references. At worse we could end up with an
            --  entity or a reference whose kind does not match an entry in the
            --  *_kind tables, and the xref will not show later on in query,
            --  but that's easily fixed by adding the new entry in the *_kind
            --  table (this occurs when the format of LI files is changed).

            DB.Execute ("PRAGMA foreign_keys=OFF;");
            DB.Execute ("PRAGMA synchronous=NORMAL;");

            if Active (Me_Use_WAL) then
               DB.Execute ("PRAGMA journal_mode=WAL;");
            end if;

            DB.Execute ("PRAGMA temp_store=MEMORY;");
            DB.Execute ("PRAGMA mmap_size=268435456;");
         end if;

         DB.Automatic_Transactions (False);
         DB.Execute ("BEGIN");

         if Destroy_Indexes then
            DB.Execute ("DROP INDEX entity_refs_entity;");
            DB.Execute ("DROP INDEX entity_refs_loc;");
            DB.Execute ("DROP INDEX e2e_from;");
            DB.Execute ("DROP INDEX e2e_to;");
            DB.Execute ("DROP INDEX entity_decl_caller;");
            DB.Execute ("DROP INDEX refs_caller;");
         end if;
      end Start_Transaction;

      ----------------------------
      -- Create_Project_Entries --
      ----------------------------

      procedure Create_Project_Entries (DB : Database_Connection) is
         Iter : Project_Iterator := Tree.Root_Project.Start;
         P    : Project_Type;
         R    : Forward_Cursor;
      begin
         loop
            P := GNATCOLL.Projects.Current (Iter);
            exit when P = No_Project;

            declare
               N  : aliased String := +P.Project_Path.Unix_Style_Full_Name
                 (Normalize => True);
               Id : Integer;
            begin
               R.Fetch (DB, Query_Get_File, (1 => +N'Unchecked_Access));
               if R.Has_Row then
                  Id := R.Integer_Value (0);
               else
                  Id := DB.Insert_And_Get_PK
                    (Query_Insert_Project_File,
                     Params => (1              => +N'Unchecked_Access),
                     PK     => Database.Files.Id);
               end if;

               Project_To_Id.Include
                 (P.Project_Path,
                  File_Db_Info'
                    (Id                  => Id,
                     Export_Mangled_Name => False));
            end;

            Next (Iter);
         end loop;
      end Create_Project_Entries;

      -----------------
      -- Parse_Files --
      -----------------

      procedure Parse_Files (DB : Database_Connection) is
         LI_C    : LI_Lists.Cursor;
         Lib     : LI_Info;
         Start   : Time;
         Dur     : Duration;
         Total   : constant Integer := Integer (LIs.Length);
         Current : Natural := 1;
         Iconv_State : Iconv_T;
      begin
         if Active (Me_Timing) then
            Start := Clock;
         end if;

         Iconv_State := Iconv_Open
           (To_Code => UTF8, From_Code => ALI_Encoding, Ignore => True);

         --  Cleanup the database by removing obsolete information. This
         --  includes:
         --    * finding source files for which the LI unit has changed (call
         --      them LI_Units)
         --    * finding all entities defined in one of those LI_Units. Call
         --      them LI_Entities).
         --    * all references to any entities within those LI_Units
         --    * all references, anywhere, to one of the LI_Entities
         --    * all e2e information from or to one of the LI_Entities
         --    * all the LI_Entities themselves.
         --    * remove f2f originating from one of the LI_Units (typically the
         --      dependencies these files have on others). After this step, we
         --      cannot recompute the list of LI_Units, so this must be last).
         --
         --  Note that this cleanup is correct, but if all the ALI files are
         --  not consistent we are losing information. For instance, a
         --  reference to an LI_Entity from a file we are not parsing again
         --  will not exist anymore, so files with obsolete ALI files will have
         --  partial xref only.
         --  Likewise for the e2e information, so we might no longer know that
         --  an entity was inheriting from one of the LI_Entities.

         --  faster to delete the indexes before we delete things
         Start_Transaction (DB, Destroy_Indexes => Destroy_Indexes);
         DB.Execute ("CREATE TEMP TABLE temp_lis (id INTEGER PRIMARY KEY);");

         LI_C := LIs.First;
         while Has_Element (LI_C) loop
            Lib := Element (LI_C);
            if Lib.Id /= -1 then
               DB.Execute
                 ("INSERT INTO temp_lis VALUES (" & Lib.Id'Img & ");");

               if Active (Me_Parsing) then
                  Trace (Me_Parsing, "Cleanup "
                         & Lib.LI.Library_File.Display_Full_Name);
               end if;
            end if;
            Next (LI_C);
         end loop;

         DB.Execute ("CREATE TEMP TABLE temp_files (id INTEGER PRIMARY KEY);");
         DB.Execute
           ("INSERT INTO temp_files SELECT DISTINCT "
            & " f2f.fromFile FROM f2f WHERE "
            & " f2f.kind = 1 AND"
            & " f2f.toFile IN (SELECT * FROM temp_lis);");
         DB.Execute ("DROP TABLE temp_lis;");
         DB.Execute
           ("CREATE TEMP TABLE temp_entities (id INTEGER PRIMARY KEY);");
         DB.Execute
           ("INSERT INTO temp_entities SELECT entities.id FROM entities,"
            & " temp.temp_files WHERE entities.decl_file=temp.temp_files.id;");
         DB.Execute   --  all import/with statements from LI_Units
           ("DELETE FROM f2f WHERE f2f.fromFile IN"
            & " (SELECT id FROM temp_files);");
         DB.Execute   --  all references within LI_Units files
           ("DELETE FROM entity_refs WHERE entity_refs.file IN"
            & " (SELECT id FROM temp_files);");
         DB.Execute ("DROP TABLE temp_files;");

         if DB.In_Transaction then
            DB.Commit_Or_Rollback;
            DB.Execute ("PRAGMA wal_checkpoint(RESTART);");
         end if;

         if Active (Me_Timing) then
            Dur := Clock - Start;
            Trace (Me_Timing,
                   "Cleaned up" & LIs.Length'Img & " files"
                   & Dur'Img & " s");
            Start := Clock;
         end if;

         Start_Transaction (DB, Destroy_Indexes => False);

         Create_Project_Entries (DB);

         --  Now process the LI files

         declare
            Prev_Root : Project_Type := No_Project;
            Lib       : LI_Info;
         begin
            LI_C := LIs.First;
            while Has_Element (LI_C) loop
               begin
                  if Show_Progress /= null then
                     Show_Progress (Cur   => Current, Total => Total);
                     Current := Current + 1;
                  end if;

                  Lib := Element (LI_C);

                  --  The Source_To_Id cache is only valid for a specific
                  --  aggregate tree, but needs to be cleared before we move
                  --  on to the next aggregated tree.
                  if Lib.LI.Non_Aggregate_Root_Project = null
                    and then Prev_Root /= No_Project
                  then
                     Prev_Root := No_Project;
                     Source_To_Id.Clear;
                  elsif Lib.LI.Non_Aggregate_Root_Project /= null
                    and then Prev_Root /=
                      Project_Type (Lib.LI.Non_Aggregate_Root_Project.all)
                  then
                     Prev_Root := Project_Type
                       (Lib.LI.Non_Aggregate_Root_Project.all);
                     Source_To_Id.Clear;
                  end if;

                  Parse_LI (DB                  => DB,
                            LI                  => Lib,
                            Default_Iconv_State => Iconv_State,
                            Source_To_Id        => Source_To_Id,
                            Project_To_Id       => Project_To_Id,
                            Visited_ALI_Units   => Visited_ALI_Units,
                            Visited_GLI_Files   => Visited_GLI_Files,
                            Entity_Decl_To_Id   => Entity_Decl_To_Id,
                            Entity_Renamings    => Entity_Renamings);
               exception
                  when E : others =>
                     Trace (Me_Error, E);
               end;
               Next (LI_C);
            end loop;
         end;

         --  ??? If we had a failure earlier, we will exit early and lost the
         --  contents of temp_entities, and therefore never clean those
         --  entities from the database until we parse the corresponding LI
         --  files again.
         DB.Execute
           ("DELETE FROM e2e WHERE e2e.fromEntity IN"
            & " (SELECT id FROM temp_entities);");
         DB.Execute
           ("DELETE FROM e2e WHERE e2e.toEntity IN"
            & " (SELECT id FROM temp_entities);");
         DB.Execute
           ("DELETE FROM entity_refs WHERE entity_refs.entity IN"
            & " (SELECT id FROM temp_entities);");
         DB.Execute
           ("DELETE FROM entities WHERE entities.id IN"
            & " (SELECT id FROM temp_entities);");
         DB.Execute ("DROP TABLE temp_entities;");

         Iconv_Close (Iconv_State);

         if Active (Me_Timing) then
            Dur := Clock - Start;
            Trace (Me_Timing,
                   "Parsed" & LIs.Length'Img & " files:"
                   & Duration'Image (Dur / Integer (LIs.Length)) & "/file,"
                   & Dur'Img & " s");
         end if;

         --  Do a commit now, so that other users (like GPS) can start
         --  querying immediately even if we haven't recreated the indexes
         --  yet.

         if Active (Me_Commit_Before_Indexes)
           and then DB.In_Transaction
         then
            DB.Commit_Or_Rollback;
            DB.Execute ("PRAGMA wal_checkpoint(RESTART);");

            --  Will need a new transaction for the indexes and renamings
            Start_Transaction (DB, Destroy_Indexes => False);
         end if;
      end Parse_Files;

      Absolute_Start : Time;
   begin
      if Active (Me_Timing) then
         Absolute_Start := Clock;
      end if;

      Project.Library_Files
        (Recursive           => True,
         Xrefs_Dirs          => True,
         Including_Libraries => True,
         ALI_Ext             => "^.*\.[ags]li$",
         List                => LI_Files,
         Include_Predefined  => Parse_Runtime_Files,
         Exclude_Overridden  => True);

      if Active (Me_Timing) then
         Trace (Me_Timing,
                "Found" & Length (LI_Files)'Img & " [ags]li files");
      end if;

      declare
         DB_Created : Boolean;
      begin
         Initialize_DB (Self, Self.DB, From_DB_Name, DB_Created,
                        Force => False);  --  From_DB_Name -> Self.DB
         if DB_Created then
            Destroy_Indexes := True;
            Do_Analyze := True;
         end if;
      end;

      Search_LI_Files_To_Update
        (Self, LI_Files, LIs, Force_Refresh => Force_Refresh);

      Destroy_Indexes :=
        Destroy_Indexes or else Length (LIs) > Indexes_Threshold;

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

               declare
                  DB_Created : Boolean;
               begin
                  Initialize_DB (Self, Memory, From_DB_Name, DB_Created,
                                 Force => True);  --  Self.DB -> :memory:
                  if DB_Created then
                     Do_Analyze := True;
                  end if;
               end;

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
     (Self  : in out Xref_Database;
      Tree  : not null GNATCOLL.Projects.Project_Tree_Access;
      DB    : not null access
        GNATCOLL.SQL.Exec.Database_Description_Record'Class;
      Error : in out GNAT.Strings.String_Access;
      Delete_If_Mismatch : Boolean := False)
   is
      R          : Forward_Cursor;
      Current_DB : Virtual_File;
      Success    : Boolean;
   begin
      Error := null;

      Self.DB := DB.Build_Connection;
      Self.Tree := Tree;

      Current_DB := Create (+SQL.Sqlite.DB_Name (Self.DB));

      if SQL.Sqlite.Is_Sqlite (Self.DB)
        and then Current_DB.Is_Regular_File
      then
         R.Fetch (Self.DB, "PRAGMA user_version;");

         if R.Integer_Value (0) /= Schema_Version
           and then R.Integer_Value (0) /= 0  --  0 means "no schema created"
         then
            Trace (Me_Debug, "Version mismatch : "
                   & R.Value (0) & " /=" & Schema_Version'Img);
            Error := new String'
              ("Database schema version is "
               & R.Value (0) & ", expecting" & Schema_Version'Img);

            if Delete_If_Mismatch then
               Self.DB.Close;
               Current_DB.Delete (Success);
               if not Success then
                  Free (Error);
                  Error := new String'
                    ("Database schema version is "
                     & R.Value (0) & ", expecting"
                     & Schema_Version'Img
                     & " and could not delete the database");
               else
                  Self.DB := DB.Build_Connection;
               end if;
            end if;
         end if;
      end if;

      Self.DB.Execute ("PRAGMA mmap_size=268435456;");

      --  Do not use automatic transactions, to avoid being stuck with an
      --  unfinished BEGIN, which would lock the database and prevents
      --  gnatinspect from running in parallel to IDEs.
      Self.DB.Automatic_Transactions (False);
   end Setup_DB;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Xref_Database) is
   begin
      if Self.DB /= null then
         Free (Self.DB);
         Self.Tree := null;
      end if;
   end Free;

   --------------------
   -- Is_Fuzzy_Match --
   --------------------

   function Is_Fuzzy_Match (Self : Entity_Information) return Boolean is
   begin
      return Self.Fuzzy;
   end Is_Fuzzy_Match;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self    : Xref_Database;
      Name    : String;
      File    : String;
      Project : Project_Type;
      Line    : Integer := -1;
      Column  : Visible_Column := -1;
      Approximate_Search_Fallback : Boolean := True) return Entity_Reference
   is
      Distance : Natural := Integer'Last;
      Best_Ref : Entity_Reference := No_Entity_Reference;
      F, P     : SQL_Criteria;

      procedure Prepare_Decl
        (C           : SQL_Criteria;
         Exact_Match : Boolean);
      --  Perform the query Q (assuming it checks the declarations).

      procedure Prepare_Ref
        (C           : SQL_Criteria;
         Exact_Match : Boolean);
      --  Perform the query Q (assuming it checks the references).

      procedure Result
        (Q : SQL_Query; Exact_Match : Boolean; From_Refs : Boolean);

      ------------
      -- Result --
      ------------

      procedure Result
        (Q : SQL_Query; Exact_Match : Boolean; From_Refs : Boolean)
      is
         R       : Forward_Cursor;
         Caller  : Entity_Information;
         Dist    : Natural;
         Kind    : Unbounded_String;
         Kind_Id : Character;
         Is_End_Of_Scope : Boolean;
      begin
         R.Fetch (Self.DB, Q, Params => (1 => +Name'Unrestricted_Access));

         if R.Has_Row then
            loop
               if Exact_Match then
                  Dist := 0;
               else
                  Dist := abs (Line - Integer_Value (R, 1))
                    + abs (Integer (Column) - Integer_Value (R, 2)) * 250;
               end if;

               if Dist < Distance then
                  if R.Is_Null (3) then
                     Caller := No_Entity;
                  else
                     Caller := (Id => R.Integer_Value (3), Fuzzy => False);
                  end if;

                  if From_Refs then
                     Kind := To_Unbounded_String (R.Value (5));
                     Kind_Id := Char_Value (R, 6);
                     Is_End_Of_Scope := Boolean_Value (R, 7);
                  else
                     Kind := To_Unbounded_String (Reference_Kind_Declaration);
                     Kind_Id := Kind_Id_Declaration;
                     Is_End_Of_Scope := False;
                  end if;

                  Best_Ref :=
                    (Entity  =>
                       (Id => R.Integer_Value (0), Fuzzy => not Exact_Match),
                     File    => Create (+R.Value (4)),
                     Project => Project,
                     Line    => R.Integer_Value (1),
                     Column  => Visible_Column (R.Integer_Value (2)),
                     Kind    => Kind,
                     Kind_Id => Kind_Id,
                     Is_End_Of_Scope => Is_End_Of_Scope,
                     Scope   => Caller);

                  Distance := Dist;
               end if;

               R.Next;

               --  For an exact match, no need to search further. An
               --  entity might have multiple references in the same
               --  location (for instance 'r' and 'm' for an 'in out'
               --  parameter).

               exit when Exact_Match or else not R.Has_Row;
            end loop;
         end if;
      end Result;

      ------------------
      -- Prepare_Decl --
      ------------------

      procedure Prepare_Decl
        (C           : SQL_Criteria;
         Exact_Match : Boolean)
      is
         Q      : SQL_Query;
      begin
         Q := SQL_Select
           (Database.Entities.Id
               & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column
               & Database.Entities.Decl_Caller
               & Database.Files.Path,
            From  => Database.Entities & Database.Files & Files3,
            Where => Database.Files.Id = Database.Entities.Decl_File
               and Database.Entities.Name = Text_Param (1)
               and Database.Files.Project = Files3.Id
               and P
               and F
               and C,
            Distinct => True,
            Limit    => 1);

         Result (Q, Exact_Match, From_Refs => False);
      end Prepare_Decl;

      -----------------
      -- Prepare_Ref --
      -----------------

      procedure Prepare_Ref
        (C           : SQL_Criteria;
         Exact_Match : Boolean)
      is
         Q      : SQL_Query;
      begin
         Q := SQL_Select
           (To_List ((
            0 => +Database.Entity_Refs.Entity,
            1 => +Database.Entity_Refs.Line,
            2 => +Database.Entity_Refs.Column,
            3 => +Database.Entity_Refs.Caller,
            4 => +Database.Files.Path,
            5 => +Database.Reference_Kinds.Display,
            6 => +Database.Reference_Kinds.Id,
            7 => +Database.Reference_Kinds.Is_End)),
            From => Database.Entity_Refs & Database.Entities & Database.Files
               & Database.Reference_Kinds & Files3,
            Where => Database.Entity_Refs.Entity = Database.Entities.Id
               and Database.Entity_Refs.File = Database.Files.Id
               and Database.Reference_Kinds.Id = Database.Entity_Refs.Kind
               and Database.Entities.Name = Text_Param (1)
               and Database.Files.Project = Files3.Id
               and P
               and F
               and C,
            Distinct => True);

         Result (Q, Exact_Match, From_Refs => True);
      end Prepare_Ref;

      C, C2 : SQL_Criteria;

   --  Start of processing for Get_Entity

   begin
      --  A trivial test that the name is a valid identifier or operation. This
      --  test must match all languages, so is necessarily crude.

      if Name'Length >= 2
        and then Name (Name'First .. Name'First + 1) = "--"
      then
         return No_Entity_Reference;
      end if;

      if GNAT.OS_Lib.Is_Absolute_Path (File) then
         F := Database.Files.Path = File;
      else
         F := Like (Database.Files.Path, "%/" & File);
      end if;

      if Project /= No_Project then
         P := Files3.Path =
           +Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
      end if;

      --  First test whether the user has passed the location of the
      --  declaration.

      if Line /= -1 then
         C := Database.Entities.Decl_Line = Line;
         C2 := Database.Entity_Refs.Line = Line;
         if Column /= -1 then
            C := C and Database.Entities.Decl_Column = Integer (Column);
            C2 := C2 and Database.Entity_Refs.Column = Integer (Column);
         end if;
      end if;

      Prepare_Decl (C, Exact_Match => True);
      if Distance = 0 then
         return Best_Ref;
      end if;

      --  Else check whether we have a matching reference

      Prepare_Ref (C2, Exact_Match => True);
      if Distance = 0 then
         return Best_Ref;
      end if;

      if Line = -1 or else not Approximate_Search_Fallback then
         return No_Entity_Reference;
      end if;

      --  Not found ? Try an approximate match on the declaration
      --  The algorithm is as follows: we find all homonym entities within
      --  a given range, and then chose the one closest to the initial
      --  location provided by the user

      declare
         use type Integer_Fields.Field;
      begin
         C := Absolute (Entities.Decl_Line - Line) < 10;
         C2 := Absolute (Entity_Refs.Line - Line) < 10;
         if Column /= -1 then
            C := C and Absolute (Entities.Decl_Column - Integer (Column)) <
               Column_Tolerance;
            C2 := C2 and Absolute (Entity_Refs.Column - Integer (Column)) <
               Column_Tolerance;
         end if;
      end;

      Prepare_Decl (C, Exact_Match => False);
      Prepare_Ref (C2, Exact_Match => False);

      --  If still not found, try to reconnect to the database. It seems that
      --  in some cases, if we connected to the database while gnatinspect was
      --  doing its work, sqlite might have cached some data in memory and
      --  incorrectly update the cache when gnatinspect commits the
      --  transation. Closing and reconnecting forces a flush of the cache.

      if False   --  ??? Temporary
         and then Best_Ref = No_Entity_Reference
         and then GNATCOLL.SQL.Sqlite.DB_Name (Self.DB) /= ":memory:"
      then
         Self.DB.Close;   --  will be reopened automatically in the next query

         Prepare_Decl (C, Exact_Match => True);
         if Distance = 0 then
            return Best_Ref;
         end if;

         --  Else check whether we have a matching reference

         Prepare_Ref (C2, Exact_Match => True);
         if Distance = 0 then
            return Best_Ref;
         end if;
      end if;

      return Best_Ref;
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Self    : Xref_Database;
      Name    : String;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type;
      Line    : Integer := -1;
      Column  : Visible_Column := -1;
      Approximate_Search_Fallback : Boolean := True) return Entity_Reference is
   begin
      return Get_Entity
        (Self, Name, +File.Unix_Style_Full_Name (Normalize => True),
         Project, Line, Column,
         Approximate_Search_Fallback => Approximate_Search_Fallback);
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
      Scope   : Entity_Information := No_Entity;
      Id      : Integer;
      Project : Project_Type := No_Project;
   begin
      if not Self.DBCursor.Has_Row then
         return No_Entity_Reference;
      end if;

      if not Self.DBCursor.Is_Null (Q_Ref_Caller) then
         Id := Self.DBCursor.Integer_Value (Q_Ref_Caller);
         if Id < 0 then
            Scope := No_Entity;
         else
            Scope := (Id => Id, Fuzzy => False);
         end if;
      end if;

      if not Self.DBCursor.Is_Null (Q_Ref_Project) then
         Project := Self.Tree.Project_From_Path
           (Create (+Self.DBCursor.Value (Q_Ref_Project)));
      end if;

      return Entity_Reference'
        (Entity  => (Id => Self.DBCursor.Integer_Value (Q_Ref_Entity),
                    Fuzzy => False),
         File    => Create (+Self.DBCursor.Value (Q_Ref_File)),
         Project => Project,
         Line    => Self.DBCursor.Integer_Value (Q_Ref_Line),
         Column  => Visible_Column (Self.DBCursor.Integer_Value (Q_Ref_Col)),
         Kind    => To_Unbounded_String (Self.DBCursor.Value (Q_Ref_Kind)),
         Kind_Id => Char_Value (Self.DBCursor, Q_Ref_Kind_Id),
         Is_End_Of_Scope =>
           Boolean_Value (Self.DBCursor, Q_Ref_Is_End_Of_Scope),
         Scope  => Scope);
   end Element;

   ----------------
   -- References --
   ----------------

   procedure References
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : in out References_Cursor'Class) is
   begin
      Cursor.Entity := Entity;
      Cursor.Tree := Self.Tree;
      Cursor.DBCursor.Fetch
        (Self.DB, Q_References, Params => (1 => +Entity.Id));
   end References;

   ----------------
   -- References --
   ----------------

   procedure References
     (Self             : Xref_Database'Class;
      Entity           : Entity_Information;
      Cursor           : in out References_Cursor'Class;
      Include_Implicit : Boolean;
      Include_All      : Boolean;
      Kinds            : String := "")
   is
      --  Use a prepared statement so that the traces show a clearer string.
      Q : SQL_Query;
      P : Prepared_Statement;

      Include_Decl : Boolean := False;
      Include_Others : Boolean := False;
   begin
      if Kinds /= "" then
         declare
            R : String_List_Access := GNATCOLL.Utils.Split (Kinds, ',');
         begin
            for K in R'Range loop
               if R (K).all = Reference_Kind_Declaration then
                  Include_Decl := True;
               else
                  Include_Others := True;
               end if;
            end loop;
            Free (R);
         end;

         if Include_Decl then
            if Include_Others then
               Q := SQL_Union
                 (SQL_Select
                    (F_References_Decl,
                     From => Database.Entities & Database.Files,
                     Where => Database.Entities.Decl_File = Database.Files.Id
                     and Database.Entities.Id = Integer_Param (1)),
                  SQL_Select
                    (F_References,
                     From => Database.Entity_Refs & Database.Files
                     & Database.Reference_Kinds,
                     Where => Database.Entity_Refs.File = Database.Files.Id
                     and Database.Entity_Refs.Kind =
                       Database.Reference_Kinds.Id
                     and SQL_In (Database.Reference_Kinds.Display, Kinds)
                     and Database.Entity_Refs.Entity = Integer_Param (1)),
                  Order_By => Database.Files.Path
                    & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
                  Distinct => True);
            else
               Q := SQL_Select
                 (F_References_Decl,
                  From => Database.Entities & Database.Files,
                  Where => Database.Entities.Decl_File = Database.Files.Id
                    and Database.Entities.Id = Integer_Param (1),
                  Order_By => Database.Files.Path
                    & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
                  Distinct => True);
            end if;
         else
            Q := SQL_Select
               (F_References,
                From => Database.Entity_Refs & Database.Files
                  & Database.Reference_Kinds,
                Where => Database.Entity_Refs.File = Database.Files.Id
                and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
                and SQL_In (Database.Reference_Kinds.Display, Kinds)
                and Database.Entity_Refs.Entity = Integer_Param (1),
                Order_By => Database.Files.Path
                  & Database.Entity_Refs.Line & Database.Entity_Refs.Column,
                Distinct => True);
         end if;

         P := Prepare
           (Q, On_Server => False, Name => "references_with_kinds");

      elsif Include_All then
         P := Q_References_All_Kinds;
      elsif Include_Implicit then
         P := Q_References;
      else
         P := Q_References_No_Implicit;
      end if;

      Cursor.Entity := Entity;
      Cursor.Tree := Self.Tree;
      Cursor.DBCursor.Fetch
        (Self.DB, P, Params => (1 => +Entity.Id));
   end References;

   -----------------
   -- Declaration --
   -----------------

   function Declaration
     (Xref   : Xref_Database;
      Entity : Entity_Information) return Entity_Declaration
   is
      Curs  : Forward_Cursor;
      Scope : Entity_Information := No_Entity;
      Id    : Integer;
   begin
      Curs.Fetch (Xref.DB, Query_Declaration, Params => (1 => +Entity.Id));

      if Curs.Has_Row then
         if not Curs.Is_Null (Q_Decl_Caller) then
            Id := Curs.Integer_Value (Q_Decl_Caller);
            if Id < 0 then
               Scope := No_Entity;
            else
               Scope := (Id => Id, Fuzzy => False);
            end if;
         end if;

         return (Name => To_Unbounded_String (Curs.Value (Q_Decl_Name)),
                 Kind => To_Unbounded_String (Curs.Value (Q_Decl_Kind)),
                 Flags =>
                   (Is_Subprogram => Curs.Boolean_Value (Q_Decl_Is_Subp),
                    Is_Container  => Curs.Boolean_Value (Q_Decl_Is_Cont),
                    Is_Abstract   => Curs.Boolean_Value (Q_Decl_Is_Abst),
                    Is_Generic    => Curs.Boolean_Value (Q_Decl_Is_Generic),
                    Is_Type => Curs.Boolean_Value (Q_Decl_Is_Type),
                    Is_Access => Curs.Boolean_Value (Q_Decl_Is_Access),
                    Is_Array => Curs.Boolean_Value (Q_Decl_Is_Array),
                    Is_Global => Curs.Boolean_Value (Q_Decl_Is_Global),
                    Has_Methods => Curs.Boolean_Value (Q_Decl_Has_Methods),
                    Is_Static_Local =>
                      Curs.Boolean_Value (Q_Decl_Is_Static_Local),
                    Is_Printable_In_Gdb =>
                      Curs.Boolean_Value (Q_Decl_Is_Printable_In_Gdb),
                    Body_Is_Full_Declaration =>
                      Curs.Boolean_Value (Q_Decl_Full_Decl)),
                 Location => (Entity => Entity,
                              File   => Create (+Curs.Value (Q_Decl_File)),
                              Line   => Curs.Integer_Value (Q_Decl_Line),
                              Column => Visible_Column
                                (Curs.Integer_Value (Q_Decl_Column)),
                              Project => Xref.Tree.Project_From_Path
                                (Create (+Curs.Value (Q_Decl_Project))),
                              Kind   => To_Unbounded_String
                                 (Reference_Kind_Declaration),
                              Kind_Id => Kind_Id_Declaration,
                              Is_End_Of_Scope => False,
                              Scope  => Scope));
      else
         return No_Entity_Declaration;
      end if;
   end Declaration;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   function Is_Predefined_Entity
     (Decl : Entity_Declaration) return Boolean is
   begin
      return Decl.Location.Line = -1;
   end Is_Predefined_Entity;

   ------------
   -- Bodies --
   ------------

   procedure Bodies
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : in out References_Cursor'Class)
   is
      E : Entity_Information;
      procedure Bodies_Internal (Body_Statement : Prepared_Statement);
      procedure Bodies_Internal (Body_Statement : Prepared_Statement) is
      begin
         Cursor.DBCursor.Fetch (Self.DB, Body_Statement,
                                Params => (1 => +Entity.Id));

         --  If we have no bodies, check whether the entity is an instantiation
         --  of another entity which itself would have a body.

         if not Has_Row (Cursor.DBCursor) then
            E := Self.Instance_Of (Entity);
            if E /= No_Entity then
               Cursor.DBCursor.Fetch (Self.DB, Body_Statement,
                                      Params => (1 => +E.Id));
            end if;
         end if;

         --  Still not found, check through renaming

         if not Has_Row (Cursor.DBCursor) then
            E := Self.Renaming_Of (Entity);
            if E /= No_Entity then
               Cursor.DBCursor.Fetch (Self.DB, Body_Statement,
                                      Params => (1 => +E.Id));
            end if;
         end if;

      end Bodies_Internal;
      Sub_Has_Elements : Boolean;
   begin
      Cursor.Entity := Entity;
      Cursor.Tree := Self.Tree;
      Cursor.DBCursor.Fetch (Self.DB, Q_Bodies_Sub_Mangled_Prep,
                             Params => (1 => +Entity.Id));

      Sub_Has_Elements := Has_Row (Cursor.DBCursor);

      if Sub_Has_Elements then
         Bodies_Internal (Q_Bodies_With_Mangled);
      else
         Bodies_Internal (Q_Bodies_Simple);
      end if;
   end Bodies;

   -------------
   -- Element --
   -------------

   function Element (Self : Entities_Cursor) return Entity_Information is
   begin
      return Entity_Information'(Id => Integer_Value (Self.DBCursor, 0),
                                Fuzzy => False);
   end Element;

   -------------
   -- Element --
   -------------

   function Element (Self : Parameters_Cursor) return Parameter_Information is
      Kind : Parameter_Kind;
   begin
      case Integer_Value (Self.DBCursor, Q_Parameters_Kind) is
         when E2e_In_Parameter     => Kind := In_Parameter;
         when E2e_Out_Parameter    => Kind := Out_Parameter;
         when E2e_In_Out_Parameter => Kind := In_Out_Parameter;
         when E2e_Access_Parameter => Kind := Access_Parameter;
         when others               => Kind := In_Parameter;
      end case;

      return Parameter_Information'
        (Parameter =>
           (Id    => Integer_Value (Self.DBCursor, Q_Parameters_Toentity),
            Fuzzy => False),
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
        (Self.DB, Query_Parameters, Params => (1 => +Entity.Id));
      return Curs;
   end Parameters;

   -------------
   -- Element --
   -------------

   function Element (Self : Files_Cursor) return GNATCOLL.VFS.Virtual_File is
   begin
      return Create (+Self.DBCursor.Value (0));
   end Element;

   -----------------
   -- Imported_By --
   -----------------

   function Imported_By
     (Self    : Xref_Database'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type) return Files_Cursor
   is
      Name  : aliased String :=
        +File.Unix_Style_Full_Name (Normalize => True);
      Project_Path  : aliased String :=
        +Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
      Curs : Files_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         SQL_Select
           (Database.Files.Path,
            From  => Database.Files & Database.F2f & Files2 & Files3,
            Where => Database.F2f.Fromfile = Database.Files.Id
              and Files2.Id = Database.F2f.Tofile
              and Files2.Path = Text_Param (1)
              and Database.F2f.Kind = F2f_Withs
              and Files2.Project = Files3.Id
              and Files3.Path = Text_Param (2),
            Order_By => Database.Files.Path),
         Params => (1 => +Name'Unchecked_Access,
                    2 => +Project_Path'Unchecked_Access));
      return Curs;
   end Imported_By;

   -------------
   -- Imports --
   -------------

   function Imports
     (Self    : Xref_Database'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type) return Files_Cursor
   is
      Name  : aliased String :=
        +File.Unix_Style_Full_Name (Normalize => True);
      Project_Path  : aliased String :=
        +Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
      Curs : Files_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         SQL_Select
           (Database.Files.Path,
            From  => Database.Files & Database.F2f & Files2 & Files3,
            Where => Database.F2f.Tofile = Database.Files.Id
              and Files2.Id = Database.F2f.Fromfile
              and Files2.Path = Text_Param (1)
              and Database.F2f.Kind = F2f_Withs
              and Files2.Project = Files3.Id
              and Files3.Path = Text_Param (2),
            Order_By => Database.Files.Path),
         Params => (1 => +Name'Unchecked_Access,
                    2 => +Project_Path'Unchecked_Access));
      return Curs;
   end Imports;

   ----------------
   -- Depends_On --
   ----------------

   function Depends_On
     (Self    : Xref_Database'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type) return File_Sets.Set
   is
      use File_Sets;
      C    : Files_Cursor := Self.Imports (File, Project);
      Seen : File_Sets.Set;
      To_Analyze : File_Sets.Set;
      F    : Virtual_File;
   begin
      while C.Has_Element loop
         To_Analyze.Include (C.Element);
         C.Next;
      end loop;

      while not To_Analyze.Is_Empty loop
         F := Element (To_Analyze.First);
         Seen.Include (F);
         To_Analyze.Delete_First;

         C := Self.Imports (F, Project);
         while C.Has_Element loop
            F := C.Element;
            if not Seen.Contains (F) then
               To_Analyze.Include (F);
            end if;
            C.Next;
         end loop;
      end loop;

      return Seen;
   end Depends_On;

   ------------------
   -- Mangled_Name --
   ------------------

   function Mangled_Name
     (Self   : Xref_Database;
      Entity : Entity_Information) return String
   is
      C : Forward_Cursor;
   begin
      C.Fetch
        (Self.DB,
         Query_Mangled_Name,
         Params => (1 => +Entity.Id));

      if C.Has_Row and not C.Is_Null (0) then
         return C.Value (0);
      else
         return "";
      end if;
   end Mangled_Name;

   --------------------
   -- Qualified_Name --
   --------------------

   function Qualified_Name
     (Self   : Xref_Database;
      Entity : Entity_Information) return String
   is
      D    : Entity_Declaration := Self.Declaration (Entity);
      Name : Unbounded_String := D.Name;
      E    : Entity_Information := D.Location.Scope;
      F    : Entity_Information;
      C    : Forward_Cursor;
      Separator : Unbounded_String := To_Unbounded_String (".");

      --------------------------
      --  In_Neverending_Loop --
      --------------------------

      function In_Neverending_Loop return Boolean;
      pragma Inline (In_Neverending_Loop);
      --  Return true if the sequence of parents repeats. This problem occurs
      --  when the compiler does not generate the scope of some parent package.

      Max_Parents : constant := 50; --  Large enough to cover all cases
      Parents : array (1 .. Max_Parents) of Integer;
      P_Count : Natural := 0;

      function In_Neverending_Loop return Boolean is
      begin
         for J in 1 .. P_Count loop
            if Parents (J) = E.Id then
               return True;
            end if;
         end loop;

         P_Count := P_Count + 1;
         Parents (P_Count) := E.Id;

         return False;
      end In_Neverending_Loop;

   begin
      if E = No_Entity then
         E := Self.Parent_Package (Entity);
      end if;

      C.Fetch
        (Self.DB,
         SQL_Select
           (Database.Files.Language,
            From  => Database.Files,
            Where => Database.Files.Path =
               (+D.Location.File.Unix_Style_Full_Name (Normalize => True))));

      if C.Has_Row then
         declare
            Language : constant String := To_Lower (C.Value (0));
         begin
            if Language = "c" or else Language = "c++" then
               Separator := To_Unbounded_String ("::");
            end if;
         end;
      end if;

      while E /= No_Entity loop
         if In_Neverending_Loop then
            return To_String (Name);
         end if;

         D := Declaration (Xref_Database'Class (Self), E);
         Name := D.Name & Separator & Name;

         F := D.Location.Scope;
         if F = No_Entity then
            --  A library-level entity, try parent packages
            F := Self.Parent_Package (E);
         end if;

         E := F;
      end loop;

      return To_String (Name);
   end Qualified_Name;

   -----------
   -- Image --
   -----------

   function Image (Kind : Parameter_Kind) return String is
   begin
      case Kind is
         when In_Parameter     => return "in";
         when Out_Parameter    => return "out";
         when In_Out_Parameter => return "in out";
         when Access_Parameter => return "access";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Self : Xref_Database; File : Virtual_File) return String
   is
      pragma Unreferenced (Self);
   begin
      return File.Display_Base_Name;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Self : Xref_Database; Ref : Entity_Reference) return String is
   begin
      return Image (Xref_Database'Class (Self), Ref.File) & ":"
        & Image (Ref.Line, Min_Width => 0)
        & ':'
        & Image (Integer (Ref.Column), Min_Width => 0);
   end Image;

   --------------
   -- Overview --
   --------------

   function Overview
     (Self   : Xref_Database;
      Entity : Entity_Information;
      Format : Formatting := Text) return String
   is
      pragma Unreferenced (Format);
      Decl   : constant Entity_Declaration :=
        Declaration (Xref_Database'Class (Self), Entity);
   begin
      return To_String
        (Decl.Kind & " declared at "
         & Image (Xref_Database'Class (Self), Decl.Location));
   end Overview;

   ---------------------
   -- Extract_Comment --
   ---------------------

   function Extract_Comment
     (Buffer            : String;
      Decl_Start_Index  : Integer;
      Decl_End_Index    : Integer;
      Language          : Language_Syntax;
      Format            : Formatting := Text;
      Look_Before_First : Boolean := True) return String
   is
      pragma Unreferenced (Format);
      Beginning, Current   : Natural;
      Result : Unbounded_String;
      Pos, Last : Integer;

      Leading_Spaces : Integer := -1;
      --  Maximum number of spaces to remove in the comments. This is
      --  computed from the first line of the comment block (by left-aligning
      --  the text on that line), and is meant to preserve indentation for
      --  later lines (in particular if they contain code sample for instance)

   begin
      if Look_Before_First then
         Get_Documentation_Before
           (Context       => Language,
            Buffer        => Buffer,
            Decl_Index    => Decl_Start_Index,
            Comment_Start => Beginning,
            Comment_End   => Current,
            Allow_Blanks  => False);

         if Beginning = 0 then
            Get_Documentation_After
              (Context       => Language,
               Buffer        => Buffer,
               Decl_Index    => Decl_End_Index,
               Comment_Start => Beginning,
               Comment_End   => Current);
         end if;

         if Beginning = 0 then
            --  In Ada, packages end of spec is after the final "end;", which
            --  is not where users put the documentation. So we also check
            --  after the beginning of the spec.

            Get_Documentation_After
              (Context       => Language,
               Buffer        => Buffer,
               Decl_Index    => Decl_Start_Index,
               Comment_Start => Beginning,
               Comment_End   => Current);
         end if;

      else
         Get_Documentation_After
           (Context       => Language,
            Buffer        => Buffer,
            Decl_Index    => Decl_End_Index,
            Comment_Start => Beginning,
            Comment_End   => Current);

         if Beginning = 0 then
            --  In Ada, packages end of spec is after the final "end;", which
            --  is not where users put the documentation. So we also check
            --  after the beginning of the spec.

            Get_Documentation_After
              (Context       => Language,
               Buffer        => Buffer,
               Decl_Index    => Decl_Start_Index,
               Comment_Start => Beginning,
               Comment_End   => Current);
         end if;

         if Beginning = 0 then
            Get_Documentation_Before
              (Context       => Language,
               Buffer        => Buffer,
               Decl_Index    => Decl_Start_Index,
               Comment_Start => Beginning,
               Comment_End   => Current,
               Allow_Blanks  => False);
         end if;
      end if;

      --  Cleanup comment marks
      if Beginning /= 0 then
         Pos := Beginning;

         if Language.Comment_Start /= null
           and then Starts_With
             (Buffer (Pos .. Current), Language.Comment_Start.all)
         then
            Pos := Pos + Language.Comment_Start'Length;
         end if;

         if Language.Comment_End /= null
           and then Ends_With
             (Buffer (Pos .. Current), Language.Comment_End.all)
         then
            Current := Current - Language.Comment_End'Length;
         end if;
         Skip_Blanks_Backward (Buffer (Pos .. Current), Current);

         while Pos <= Current loop
            Last := EOL (Buffer (Pos .. Current - 1));

            if Language.New_Line_Comment_Start /= null then
               Skip_Blanks (Buffer, Pos);

               if Starts_With (Buffer (Pos .. Buffer'Last),
                               Language.New_Line_Comment_Start.all)
               then
                  Pos := Pos + Language.New_Line_Comment_Start'Length;
               end if;

               if Leading_Spaces = -1 then
                  --  First line, compute the number of spaces

                  Leading_Spaces := Pos;
                  Skip_Blanks (Buffer (Pos .. Buffer'Last), Pos);
                  Leading_Spaces := Pos - Leading_Spaces;
               end if;

               --  Remove leading blanks, to preserve indented code in the
               --  comments.
               for N in 1 .. Leading_Spaces loop
                  if Pos <= Buffer'Last
                    and then Buffer (Pos) = ' '
                  then
                     Pos := Pos + 1;
                  else
                     exit;
                  end if;
               end loop;
            end if;

            if Pos = Last then
               Append (Result, ASCII.LF);
            else
               Append (Result, Buffer (Pos .. Last));
            end if;
            Pos := Last + 1;
         end loop;

         return To_String (Result);
      else
         return "";
      end if;
   end Extract_Comment;

   ---------------------
   -- Extract_Comment --
   ---------------------

   function Extract_Comment
     (Buffer            : String;
      Decl_Start_Line   : Integer;
      Decl_Start_Column : Integer;
      Decl_End_Line     : Integer := -1;
      Decl_End_Column   : Integer := -1;
      Language          : Language_Syntax;
      Format            : Formatting := Text;
      Look_Before_First : Boolean := True) return String
   is
      Start, Last, Skipped : Integer;
   begin
      Start := Buffer'First;
      Skip_Lines
        (Buffer,
         Lines         => Decl_Start_Line - 1,
         Index         => Start,
         Lines_Skipped => Skipped);
      if Skipped /= Decl_Start_Line - 1 then
         return "";
      end if;

      Skip_To_Column
        (Buffer,
         Columns => Decl_Start_Column - 1,
         Index   => Start);

      Last := Start;

      if Decl_End_Line /= -1 then
         Skip_Lines
           (Buffer,
            Lines         => Decl_End_Line - Decl_Start_Line,
            Index         => Last,
            Lines_Skipped => Skipped);

         if Decl_End_Column /= -1 then
            Skip_To_Column (Buffer, Decl_End_Column, Last);
         end if;
      end if;

      return Extract_Comment
        (Buffer            => Buffer,
         Decl_Start_Index  => Start,
         Decl_End_Index    => Last,
         Language          => Language,
         Format            => Format,
         Look_Before_First => Look_Before_First);
   end Extract_Comment;

   -------------
   -- Comment --
   -------------

   function Comment
     (Self     : Xref_Database;
      Entity   : Entity_Information;
      Language : Language_Syntax;
      Format   : Formatting := Text;
      Look_Before_First : Boolean := True) return String
   is
      Buffer : GNAT.Strings.String_Access;
      Decl   : constant Entity_Declaration :=
        Declaration (Xref_Database'Class (Self), Entity);
      R : Forward_Cursor;
   begin
      if Decl = No_Entity_Declaration then
         return "";
      end if;

      Buffer := Decl.Location.File.Read_File;
      if Buffer /= null then
         R.Fetch (Self.DB, Q_End_Of_Spec, Params => (1 => +Entity.Id));
         if R.Has_Row then
            declare
               Result : constant String := Extract_Comment
                 (Buffer            => Buffer.all,
                  Decl_Start_Line   => Decl.Location.Line,
                  Decl_Start_Column => Integer (Decl.Location.Column),
                  Decl_End_Line     => R.Integer_Value (0),
                  Decl_End_Column   => R.Integer_Value (1),
                  Language          => Language,
                  Format            => Format,
                  Look_Before_First => Look_Before_First);
            begin
               if Result /= "" then
                  Free (Buffer);
                  return Result;
               end if;
            end;

         else
            declare
               Result : constant String := Extract_Comment
                 (Buffer            => Buffer.all,
                  Decl_Start_Line   => Decl.Location.Line,
                  Decl_Start_Column => Integer (Decl.Location.Column),
                  Language          => Language,
                  Format            => Format,
                  Look_Before_First => Look_Before_First);
            begin
               if Result /= "" then
                  Free (Buffer);
                  return Result;
               end if;
            end;
         end if;

         Free (Buffer);
      end if;

      --  No comment next to the spec, try the body.
      --  In particular, in C, it is rare to have the doc next to an "extern"
      --  declaration, and the doc will often be with the body instead.

      declare
         Curs : References_Cursor;
         Ref  : Entity_Reference;
      begin
         Self.Bodies (Entity, Curs);
         while Has_Element (Curs) loop
            Ref := Element (Curs);

            Buffer := Ref.File.Read_File;
            if Buffer /= null then
               declare
                  Result : constant String := Extract_Comment
                    (Buffer            => Buffer.all,
                     Decl_Start_Line   => Ref.Line,
                     Decl_Start_Column => Integer (Ref.Column),
                     Language          => Language,
                     Format            => Format,
                     Look_Before_First => Look_Before_First);
               begin
                  if Result /= "" then
                     Free (Buffer);
                     return Result;
                  end if;
               end;

               Free (Buffer);
            end if;

            Next (Curs);
         end loop;
      end;

      return "";
   end Comment;

   ----------------------
   -- Text_Declaration --
   ----------------------

   function Text_Declaration
     (Self   : Xref_Database;
      Entity : Entity_Information;
      Format : Formatting := Text) return String
   is
      pragma Unreferenced (Format);

      P : Parameters_Cursor := Parameters (Xref_Database'Class (Self), Entity);
      Param  : Parameter_Information;
      Result : Unbounded_String;
      Typ    : Entity_Information;
      Decl   : Entity_Declaration;
   begin
      if P.Has_Element then
         Append (Result, "Parameters" & ASCII.LF);

         while P.Has_Element loop
            Param := P.Element;
            Decl := Declaration (Xref_Database'Class (Self), Param.Parameter);
            Append
              (Result, "   "
               & Decl.Name
               & " : " & Image (Param.Kind)
               & " "
               & Qualified_Name
                 (Xref_Database'Class (Self), Self.Type_Of (Param.Parameter))
               & ASCII.LF);
            P.Next;
         end loop;
      end if;

      Typ := Self.Type_Of (Entity);
      if Typ /= No_Entity then
         Decl := Declaration (Xref_Database'Class (Self), Entity);
         if Decl.Flags.Is_Subprogram then
            Append (Result, "Returns" & ASCII.LF);
         else
            Append (Result, "Type" & ASCII.LF);
         end if;

         Append (Result, "   "
                 & Qualified_Name (Xref_Database'Class (Self), Typ)
                 & ASCII.LF);
      end if;

      return To_String (Result);
   end Text_Declaration;

   -------------------
   -- Documentation --
   -------------------

   function Documentation
     (Self     : Xref_Database;
      Entity   : Entity_Information;
      Language : Language_Syntax;
      Format   : Formatting := Text;
      Look_Before_First : Boolean := True) return String
   is
      Result : Unbounded_String;
      D       : constant Entity_Declaration := Self.Declaration (Entity);
      Mangled : constant String := Self.Mangled_Name (Entity);
      Comm    : constant String :=
        Comment (Xref_Database'Class (Self), Entity, Language, Format,
                 Look_Before_First => Look_Before_First);
      Decl    : constant String :=
        Text_Declaration (Xref_Database'Class (Self), Entity, Format);
   begin
      --  Only output the documentation, not the overview for the entity.
--        Result := To_Unbounded_String
--          (Qualified_Name (Xref_Database'Class (Self), Entity) & ASCII.LF);
--        Append
--          (Result,
--           Overview (Xref_Database'Class (Self), Entity, Format) & ASCII.LF);

      if Comm /= "" then
         Append (Result, Comm & ASCII.LF);
      end if;

      if Decl /= "" then
         Append (Result, Decl & ASCII.LF);
      end if;

      if Mangled /= "" and then Mangled /= D.Name then
         Append (Result, "Mangled name: " & Mangled & ASCII.LF);
      end if;

      return To_String
        (Ada.Strings.Unbounded.Trim
           (Result,
            Left => Ada.Strings.Maps.Null_Set,
            Right => Ada.Strings.Maps.To_Set
              (' ' & ASCII.HT & ASCII.LF & ASCII.CR)));
   end Documentation;

   -----------
   -- Calls --
   -----------

   procedure Calls
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class)
   is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB, Q_Calls, Params => (1 => +Entity.Id));
   end Calls;

   -------------
   -- Callers --
   -------------

   procedure Callers
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         SQL_Select
           (Entities2.Id
            & Entities2.Name
            & Entities2.Decl_Line
            & Entities2.Decl_Column,

            From => Entities2 & Database.Entities & Database.Entity_Refs
               & Database.Reference_Kinds,
            Where => Entities2.Id = Database.Entity_Refs.Caller
               and Database.Entity_Refs.Kind = Database.Reference_Kinds.Id
               and Database.Reference_Kinds.Is_Real
               and Database.Entity_Refs.Entity = Integer_Param (1)
               and Entities2.Id /= Database.Entity_Refs.Entity,

            Order_By =>
              Entities2.Name & Entities2.Decl_Line & Entities2.Decl_Column,
            Distinct => True),
         Params => (1 => +Entity.Id));
   end Callers;

   -----------------
   -- Child_Types --
   -----------------

   procedure Child_Types
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_To,
         Params => (1 => +Entity.Id, 2 => +E2e_Parent_Type));
   end Child_Types;

   ------------------
   -- Parent_Types --
   ------------------

   procedure Parent_Types
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Parent_Type));
   end Parent_Types;

   -------------
   -- Methods --
   -------------

   procedure Methods
     (Self              : Xref_Database'Class;
      Entity            : Entity_Information;
      Cursor            : out Entities_Cursor'Class;
      Include_Inherited : Boolean := True) is
   begin
      if Include_Inherited then
         Cursor.DBCursor.Fetch
           (Self.DB,
            Query_E2E_From,
            Params => (1 => +Entity.Id, 2 => +E2e_Has_Primitive));
      else
         Cursor.DBCursor.Fetch
           (Self.DB,
            Query_Non_Inherited_Methods,
            Params => (1 => +Entity.Id));
      end if;
   end Methods;

   ---------------------
   -- Discriminant_Of --
   ---------------------

   function Discriminant_Of
      (Self   : Xref_Database'Class;
       Entity : Entity_Information) return Entity_Information
   is
      Curs : Entities_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         Query_E2E_To,
         Params => (1 => +Entity.Id, 2 => +E2e_Has_Discriminant));

      if Curs.DBCursor.Has_Row then
         return Curs.Element;
      else
         return No_Entity;
      end if;
   end Discriminant_Of;

   --------------------
   -- Parent_Package --
   --------------------

   function Parent_Package
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information
   is
      Curs : Entities_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Parent_Package));

      if Curs.DBCursor.Has_Row then
         return Curs.Element;
      else
         return No_Entity;
      end if;
   end Parent_Package;

   -------------------
   -- Discriminants --
   -------------------

   procedure Discriminants
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Has_Discriminant));
   end Discriminants;

   ------------
   -- Fields --
   ------------

   procedure Fields
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB, Query_Fields, Params => (1 => +Entity.Id));
   end Fields;

   --------------
   -- Literals --
   --------------

   procedure Literals
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class) is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_To,
         Params => (1 => +Entity.Id, 2 => +E2e_From_Enumeration));
   end Literals;

   -----------------
   -- Index_Types --
   -----------------

   procedure Index_Types
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class)
   is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Has_Index));
   end Index_Types;

   -----------------------
   -- Formal_Parameters --
   -----------------------

   procedure Formal_Parameters
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class)
   is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Is_Formal_Of));
   end Formal_Parameters;

   ---------------
   -- Method_Of --
   ---------------

   procedure Method_Of
      (Self   : Xref_Database'Class;
       Entity : Entity_Information;
       Cursor : out Entities_Cursor'Class)
   is
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         Query_E2E_To,
         Params => (1 => +Entity.Id, 2 => +E2e_Has_Primitive));
   end Method_Of;

   -----------------
   -- Instance_Of --
   -----------------

   function Instance_Of
      (Self   : Xref_Database'Class;
       Entity : Entity_Information) return Entity_Information
   is
      Curs : Entities_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e_Instance_Of));

      if Curs.DBCursor.Has_Row then
         return Curs.Element;
      else
         return No_Entity;
      end if;
   end Instance_Of;

   -------------------
   -- Overridden_By --
   -------------------

   procedure Overridden_By
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : out Entities_Cursor'Class)
   is
      S : Entity_Information;
   begin
      Cursor.DBCursor.Fetch
        (Self.DB, Query_E2E_To,
         Params => (1 => +Entity.Id, 2 => +E2e_Overrides));

      --  If the entity does not have direct overridings, it might be the
      --  parameter of a subprogram. In this case, we find to find the
      --  homonym parameters in the overridden subprograms (since renaming the
      --  original parameter might require

      if not Cursor.DBCursor.Has_Row then
         S := Self.Parameter_Of (Entity);
         if S /= No_Entity then
            Cursor.DBCursor.Fetch
              (Self.DB, Query_Overriding_Parameters,
               Params => (1 => +Entity.Id, 2 => +S.Id));
         end if;
      end if;
   end Overridden_By;

   ----------------------------
   -- Single_Entity_From_E2e --
   ----------------------------

   function Single_Entity_From_E2e
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      E2e    : Integer) return Entity_Information
   is
      Curs : Entities_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB,
         Query_E2E_From,
         Params => (1 => +Entity.Id, 2 => +E2e));

      if Curs.DBCursor.Has_Row then
         return Curs.Element;
      else
         return No_Entity;
      end if;
   end Single_Entity_From_E2e;

   -------------
   -- Type_Of --
   -------------

   function Type_Of
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information
   is
      Result : Entity_Information;
   begin
      Result := Single_Entity_From_E2e (Self, Entity, E2e_Of_Type);
      if Result = No_Entity then
         Result := Single_Entity_From_E2e
           (Self, Entity, E2e_From_Enumeration);
      end if;
      return Result;
   end Type_Of;

   -----------------
   -- Renaming_Of --
   -----------------

   function Renaming_Of
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information is
   begin
      return Single_Entity_From_E2e (Self, Entity, E2e_Renames);
   end Renaming_Of;

   ------------------
   -- Parameter_Of --
   ------------------

   function Parameter_Of
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information
   is
      Curs : Entities_Cursor;
   begin
      Curs.DBCursor.Fetch
        (Self.DB, Query_Parameter_Of, Params => (1 => +Entity.Id));

      if Curs.DBCursor.Has_Row then
         return Curs.Element;
      else
         return No_Entity;
      end if;
   end Parameter_Of;

   --------------------
   -- Component_Type --
   --------------------

   function Component_Type
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information is
   begin
      return Single_Entity_From_E2e (Self, Entity, E2e_Component_Type);
   end Component_Type;

   ---------------
   -- Overrides --
   ---------------

   function Overrides
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information
   is
      E, S : Entity_Information;
      Curs : Entities_Cursor;
   begin
      E := Single_Entity_From_E2e (Self, Entity, E2e_Overrides);

      --  Handles overriding parameters
      if E = No_Entity then
         S := Self.Parameter_Of (Entity);
         if S /= No_Entity then
            Curs.DBCursor.Fetch
              (Self.DB, Query_Overridden_Parameters,
               Params => (1 => +Entity.Id, 2 => +S.Id));
            if Curs.DBCursor.Has_Row then
               E := Curs.Element;
            end if;
         end if;
      end if;

      return E;
   end Overrides;

   ------------------
   -- Pointed_Type --
   ------------------

   function Pointed_Type
     (Self   : Xref_Database'Class;
      Entity : Entity_Information) return Entity_Information
   is
   begin
      return Single_Entity_From_E2e (Self, Entity, E2e_Pointed_Type);
   end Pointed_Type;

   ----------------
   -- References --
   ----------------

   procedure References
      (Self   : Xref_Database'Class;
       File   : GNATCOLL.VFS.Virtual_File;
       Cursor : out References_Cursor'Class;
       Kind   : String := "";
       Sort   : References_Sort := By_Location)
   is
      N : aliased String :=
         +File.Unix_Style_Full_Name (Normalize => True);
      K : aliased String := Kind;
   begin
      Cursor.Entity := No_Entity;
      Cursor.Tree := Self.Tree;

      if Kind = Reference_Kind_Declaration then
         Cursor.DBCursor.Fetch
           (Self.DB, Q_File_References_Decl_By_Loc,
            Params => (1 => +N'Unchecked_Access));
      elsif Kind /= "" then
         if Sort = By_Location then
            Cursor.DBCursor.Fetch
              (Self.DB, Q_File_References_And_Kind_By_Loc,
               Params => (1 => +N'Unchecked_Access, 2 => +K'Unchecked_Access));
         else
            Cursor.DBCursor.Fetch
              (Self.DB, Q_File_References_And_Kind_By_Entity,
               Params => (1 => +N'Unchecked_Access, 2 => +K'Unchecked_Access));
         end if;
      else
         if Sort = By_Location then
            Cursor.DBCursor.Fetch
              (Self.DB, Q_File_References_By_Loc,
               Params => (1 => +N'Unchecked_Access));
         else
            Cursor.DBCursor.Fetch
              (Self.DB, Q_File_References_By_Entity,
               Params => (1 => +N'Unchecked_Access));
         end if;
      end if;
   end References;

   -------------------
   -- Referenced_In --
   -------------------

   procedure Referenced_In
     (Self    : Xref_Database'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type;
      Cursor  : out Entities_Cursor'Class)
   is
      Project_N : aliased constant String :=
        +Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         SQL_Union
           (SQL_Select
              (Database.Entities.Id
               & Database.Files.Path
               & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
               From => Database.Entities & Database.Files & Files3,
               Where => Database.Entities.Decl_File = Database.Files.Id
                 and Like (Database.Files.Path,
                   +File.Unix_Style_Full_Name (Normalize => True))
                 and Database.Files.Project = Files3.Id
                 and Files3.Path = Text_Param (1)),

            SQL_Select
              (Database.Entities.Id
               & Database.Files.Path
               & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
               From => Database.Entity_Refs & Database.Files & Files3
                  & Database.Entities,
               Where => Database.Entity_Refs.File = Database.Files.Id
                  and Like (Database.Files.Path,
                            +File.Unix_Style_Full_Name (Normalize => True))
                  and Database.Entity_Refs.Entity = Database.Entities.Id
                  and Database.Files.Project = Files3.Id
                  and Files3.Path = Text_Param (1)),

            Order_By => Database.Files.Path & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
            Distinct => True),

        Params => (1 => +Project_N'Unchecked_Access));
   end Referenced_In;

   -------------------
   -- Referenced_In --
   -------------------

   procedure Referenced_In
     (Self    : Xref_Database'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type;
      Name    : String;
      Cursor  : out Entities_Cursor'Class)
   is
      NName  : aliased String := Name;
      Name_A : constant Access_String := NName'Unchecked_Access;
      Project_N : aliased constant String :=
        +Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
   begin
      Cursor.DBCursor.Fetch
        (Self.DB,
         SQL_Union
           (SQL_Select
              (Database.Entities.Id
               & Database.Files.Path
               & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
               From => Database.Entities & Database.Files & Files3,
               Where => Database.Entities.Decl_File = Database.Files.Id
                 and Like (Database.Files.Path,
                         +File.Unix_Style_Full_Name (Normalize => True))
                 and Database.Entities.Name = Text_Param (1)
                 and Database.Files.Project = Files3.Id
                 and Files3.Path = Text_Param (2)),

            SQL_Select
              (Database.Entities.Id
               & Database.Files.Path
               & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
               From => Database.Entity_Refs & Database.Files & Files3
                  & Database.Entities,
               Where => Database.Entity_Refs.File = Database.Files.Id
                 and Like (Database.Files.Path,
                           +File.Unix_Style_Full_Name (Normalize => True))
                 and Database.Entity_Refs.Entity = Database.Entities.Id
                 and Database.Entities.Name = Text_Param (1)
                 and Database.Files.Project = Files3.Id
                and Files3.Path = Text_Param (2)),

            Order_By => Database.Files.Path & Database.Entities.Decl_Line
               & Database.Entities.Decl_Column,
            Distinct => True),

         Params => (1 => +Name_A, 2 => +Project_N'Unchecked_Access));
   end Referenced_In;

   ---------
   -- "<" --
   ---------

   function "<" (E1, E2 : Entity_Information) return Boolean is
   begin
      return E1.Id < E2.Id;
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (E1, E2 : Entity_Information) return Boolean is
   begin
      return E1.Id = E2.Id;
   end "=";

   ---------------------
   -- Init_For_Entity --
   ---------------------

   procedure Init_For_Entity
     (Self   : in out Recursive_References_Cursor'Class;
      Entity : Entity_Information)
   is
      Rename : Entity_Information;
      Ents   : Entities_Cursor;
   begin
      Self.Entity := Entity;

      if Entity = No_Entity then
         return;
      end if;

      Self.Visited.Include (Entity);

      if Self.From_Renames then
         Rename := Self.Xref.Renaming_Of (Entity);
         if Rename /= No_Entity
           and then not Self.Visited.Contains (Rename)
         then
            Self.To_Visit.Include (Rename);
         end if;
      end if;

      if Self.From_Overriding then
         Self.Xref.Overridden_By (Entity, Cursor => Ents);
         while Ents.Has_Element loop
            Rename := Ents.Element;
            if not Self.Visited.Contains (Rename) then
               Self.To_Visit.Include (Rename);
            end if;
            Ents.Next;
         end loop;
      end if;

      if Self.From_Overridden then
         Rename := Self.Xref.Overrides (Entity);
         if Rename /= No_Entity
           and then not Self.Visited.Contains (Rename)
         then
            Self.To_Visit.Include (Rename);
         end if;
      end if;

      --  Sets Self.DBCursor
      Self.Compute (Self.Xref.all, Entity => Entity, Cursor => Self);
   end Init_For_Entity;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Recursive_References_Cursor) is
      Ent : Entity_Information;
   begin
      Next (References_Cursor (Self));
      if not Self.Has_Element
        and then not Self.To_Visit.Is_Empty
      then
         Ent := Self.To_Visit.First_Element;
         Self.To_Visit.Delete_First;
         Init_For_Entity (Self, Ent);
      end if;
   end Next;

   ---------------
   -- Recursive --
   ---------------

   procedure Recursive
     (Self            : access Xref_Database'Class;
      Entity          : Entity_Information;
      Compute         : Reference_Iterator;
      Cursor          : out Recursive_References_Cursor'Class;
      From_Overriding : Boolean := True;
      From_Overridden : Boolean := True;
      From_Renames    : Boolean := True)
   is
   begin
      Cursor.Tree            := Self.Tree;
      Cursor.Compute         := Compute;
      Cursor.From_Overridden := From_Overridden;
      Cursor.From_Overriding := From_Overriding;
      Cursor.From_Renames    := From_Renames;
      Cursor.Xref            := Xref_Database_Access (Self);
      Cursor.Visited         := Entity_Sets.Empty_Set;
      Cursor.To_Visit        := Entity_Sets.Empty_Set;
      Init_For_Entity (Cursor, Entity);
   end Recursive;

   ---------------------
   -- Init_For_Entity --
   ---------------------

   procedure Init_For_Entity
     (Self   : in out Recursive_Entities_Cursor'Class;
      Entity : Entity_Information)
   is
   begin
      Self.Visited.Include (Entity);
      Self.Compute (Self.Xref.all, Entity, Cursor => Self);
      if Self.Has_Element then
         if not Self.Visited.Contains (Self.Element) then
            Self.To_Visit.Include (Self.Element);
         end if;
      end if;
   end Init_For_Entity;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Recursive_Entities_Cursor) is
      Entity : Entity_Information;
   begin
      Next (Entities_Cursor (Self));
      if Self.Has_Element then
         Entity := Self.Element;
         if not Self.Visited.Contains (Entity) then
            Self.To_Visit.Include (Entity);
         end if;
      elsif not Self.To_Visit.Is_Empty then
         Entity := Self.To_Visit.First_Element;
         Self.To_Visit.Delete_First;
         Init_For_Entity (Self, Entity);
      end if;
   end Next;

   ---------------
   -- Recursive --
   ---------------

   procedure Recursive
     (Self    : access Xref_Database'Class;
      Entity  : Entity_Information;
      Compute : Entities_Iterator;
      Cursor  : out Recursive_Entities_Cursor)
   is
   begin
      Cursor.Compute         := Compute;
      Cursor.Xref            := Xref_Database_Access (Self);
      Cursor.Visited         := Entity_Sets.Empty_Set;
      Cursor.To_Visit        := Entity_Sets.Empty_Set;
      Init_For_Entity (Cursor, Entity);
   end Recursive;

   ------------------------------
   -- Get_Documentation_Before --
   ------------------------------

   procedure Get_Documentation_Before
     (Context       : Language_Syntax;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Allow_Blanks  : Boolean := False) is
   begin
      Comment_Start := Decl_Index;
      Skip_To_Previous_Comment_Start
        (Context, Buffer, Comment_Start, Allow_Blanks);
      Comment_End := Comment_Start;

      if Comment_Start /= 0 then
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Me_Debug) then
            Trace (Me_Debug,
                   "Get_Documentation: Found a comment before the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_Before;

   -----------------------------
   -- Get_Documentation_After --
   -----------------------------

   procedure Get_Documentation_After
     (Context       : Language_Syntax;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural)
   is
   begin
      --  Else look after the comment after the declaration (which is the
      --  first block of comments after the declaration line, and not
      --  separated by a blank line)
      Comment_Start := Decl_Index;
      Skip_To_Next_Comment_Start (Context, Buffer, Comment_Start);
      Comment_End := Comment_Start;

      if Comment_Start /= 0 then
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Me_Debug) then
            Trace (Me_Debug,
                   "Get_Documentation: Found a comment after the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_After;

   ---------------------------------
   -- Looking_At_Start_Of_Comment --
   ---------------------------------

   function Looking_At_Start_Of_Comment
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : Natural) return Comment_Type is
   begin
      if Context.New_Line_Comment_Start /= null
        and then Index + Context.New_Line_Comment_Start'Length <= Buffer'Last
        and then Buffer
          (Index .. Index + Context.New_Line_Comment_Start'Length - 1) =
          Context.New_Line_Comment_Start.all
      then
         return Comment_Single_Line;
      end if;

      if Context.New_Line_Comment_Start_Regexp /= null
        and then Match (Context.New_Line_Comment_Start_Regexp.all,
                        Buffer, Data_First => Index)
      then
         return Comment_Single_Line;
      end if;

      if Context.Comment_Start /= null
        and then Index + Context.Comment_Start'Length <= Buffer'Last
        and then Buffer (Index .. Index + Context.Comment_Start'Length - 1)
        = Context.Comment_Start.all
      then
         return Comment_Multi_Line;
      end if;

      return No_Comment;
   end Looking_At_Start_Of_Comment;

   -----------------------------------------
   -- Skip_To_Current_Comment_Block_Start --
   -----------------------------------------

   procedure Skip_To_Current_Comment_Block_Start
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : in out Natural)
   is
      Initial_Index : constant Natural := Index;
      Lines_Skipped : Natural;
      Tmp           : Integer;

      function Only_Blanks_Before
        (Buffer : String;
         Index  : Natural)
         return Boolean;
      --  Return True if there are only blanks characters before the one
      --  pointed by Index in Buffer.
      --  Return False otherwise.

      ------------------------
      -- Only_Blanks_Before --
      ------------------------

      function Only_Blanks_Before
        (Buffer : String;
         Index  : Natural)
         return Boolean
      is
         Tmp : Natural := Index - 1;
      begin
         Skip_Blanks_Backward (Buffer, Tmp);
         return Buffer'First = Tmp + 1;
      end Only_Blanks_Before;

   begin
      --  Are we in a multi-line comment ?

      if Context.Comment_End /= null then
         Tmp := Line_End (Buffer, Index);

         if Tmp - Context.Comment_End'Length + 1 >= Index
           and then Buffer
             (Tmp - Context.Comment_End'Length + 1 .. Tmp) =
             Context.Comment_End.all
         then -- The end of a multi-line comment has been found
            while Index >= Buffer'First
              and then Buffer
                (Index .. Index + Context.Comment_Start'Length - 1) /=
                Context.Comment_Start.all
            loop
               Index := Index - 1;
            end loop;

            if Looking_At_Start_Of_Comment (Context, Buffer, Index) =
              Comment_Multi_Line
            then -- The beginning of a multi-line comment has been found
               return;
            end if;
         end if;
      end if;

      --  Check for single line comments

      Tmp := Initial_Index;

      loop
         while Tmp <= Buffer'Last
           and then (Buffer (Tmp) = ' ' or else Buffer (Tmp) = ASCII.HT)
         loop
            Tmp := Tmp + 1;
         end loop;

         exit when Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
           No_Comment;

         Index := Tmp;

         exit when Only_Blanks_Before (Buffer, Tmp);

         Skip_Lines (Buffer, -1, Tmp, Lines_Skipped);

         exit when Lines_Skipped /= 1;
      end loop;

      if Looking_At_Start_Of_Comment (Context, Buffer, Index) = No_Comment then
         Index := 0;
      end if;
   end Skip_To_Current_Comment_Block_Start;

   ---------------------------------------
   -- Skip_To_Current_Comment_Block_End --
   ---------------------------------------

   procedure Skip_To_Current_Comment_Block_End
     (Context            : Language_Syntax;
      Buffer             : String;
      Index              : in out Natural;
      Ignore_Blank_Lines : Boolean := False)
   is
      Last_Comment_Index : Integer := Index;
      Typ                : Comment_Type;
      Lines_Skipped      : Natural;
   begin
      Block_Iteration : loop
         Typ := Looking_At_Start_Of_Comment (Context, Buffer, Index);

         case Typ is
         when No_Comment =>
            Index := Last_Comment_Index;
            exit Block_Iteration;

         when Comment_Single_Line =>
            Index := Line_End (Buffer, Index);

            declare
               Tmp : Integer := Index;
            begin
               loop
                  Skip_Lines (Buffer, 1, Tmp, Lines_Skipped);

                  exit when Lines_Skipped /= 1;

                  while Tmp <= Buffer'Last
                    and then (Buffer (Tmp) = ' ' or Buffer (Tmp) = ASCII.HT)
                  loop
                     Tmp := Tmp + 1;
                  end loop;

                  exit when
                    Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
                    No_Comment;

                  Index := Tmp;
               end loop;
            end;

         when Comment_Multi_Line =>
            Skip_To_String (Buffer, Index, Context.Comment_End.all);
            Index := Index - 1;
         end case;

         if Ignore_Blank_Lines then
            Last_Comment_Index := Index;
            Skip_Lines (Buffer, 1, Index, Lines_Skipped);

            exit Block_Iteration when Lines_Skipped /= 1;

            Skip_Blanks (Buffer, Index);
         else
            exit Block_Iteration;
         end if;

      end loop Block_Iteration;
   end Skip_To_Current_Comment_Block_End;

   --------------------------------
   -- Skip_To_Next_Comment_Start --
   --------------------------------

   procedure Skip_To_Next_Comment_Start
     (Context : Language_Syntax;
      Buffer  : String;
      Index   : in out Natural)
   is
      Lines_Skipped : Natural;
   begin
      while Index < Buffer'Last loop
         Skip_Lines (Buffer, 1, Index, Lines_Skipped);

         exit when Lines_Skipped /= 1 or else Is_Blank_Line (Buffer, Index);

         Skip_Blanks (Buffer, Index);

         if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
           No_Comment
         then
            return;
         end if;
      end loop;

      Index := 0;
   end Skip_To_Next_Comment_Start;

   ------------------------------------
   -- Skip_To_Previous_Comment_Start --
   ------------------------------------

   procedure Skip_To_Previous_Comment_Start
     (Context      : Language_Syntax;
      Buffer       : String;
      Index        : in out Natural;
      Allow_Blanks : Boolean := False)
   is
      Lines_Skipped   : Natural;
      No_Blanks       : Boolean := not Allow_Blanks;
      Non_Blank_Found : Boolean := False;
   begin
      if Index = Buffer'First then
         Skip_Blanks (Buffer, Index);

         if Looking_At_Start_Of_Comment
             (Context, Buffer, Index) /= No_Comment
         then
            Skip_To_Current_Comment_Block_Start (Context, Buffer, Index);
         else
            Index := 0;
         end if;

         return;
      end if;

      loop
         Skip_Lines (Buffer, -1, Index, Lines_Skipped);

         exit when Lines_Skipped /= 1;

         if Is_Blank_Line (Buffer, Index) then
            exit when No_Blanks;
         else
            if Non_Blank_Found then
               --  No longer allow blank lines after a first non blank one
               No_Blanks := True;
            else
               Non_Blank_Found := True;
            end if;

            Skip_Blanks (Buffer, Index);

            if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
              No_Comment
            then
               Skip_To_Current_Comment_Block_Start (Context, Buffer, Index);
               return;
            end if;
         end if;
      end loop;

      Index := 0;
   end Skip_To_Previous_Comment_Start;

   ----------------
   -- Char_Value --
   ----------------

   function Char_Value
     (R : Forward_Cursor; Field : Field_Index) return Character
   is
      T : constant String := R.Value (Field);
   begin
      if T = "" then
         return Kind_Id_Declaration;
      else
         return T (T'First);
      end if;
   end Char_Value;

   -----------------------
   -- Show_In_Callgraph --
   -----------------------

   function Show_In_Callgraph
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return True;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Show_In_Callgraph,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Show_In_Callgraph;

   --------------------------------
   -- Is_Read_Or_Write_Reference --
   --------------------------------

   function Is_Read_Or_Write_Reference
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return True;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Real
               & Database.Reference_Kinds.Is_Write,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0) or else Boolean_Value (R, 1);
      end if;
   end Is_Read_Or_Write_Reference;

   -----------------------
   -- Is_Real_Reference --
   -----------------------

   function Is_Real_Reference
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return True;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Real,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Is_Real_Reference;

   -----------------------
   -- Is_Read_Reference --
   -----------------------

   function Is_Read_Reference
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return True;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Read,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Is_Read_Reference;

   ---------------------------
   -- Is_Implicit_Reference --
   ---------------------------

   function Is_Implicit_Reference
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return True;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Implicit,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Is_Implicit_Reference;

   ------------------------
   -- Is_Write_Reference --
   ------------------------

   function Is_Write_Reference
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return False;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Write,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Is_Write_Reference;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call
     (Xref : Xref_Database;
      Ref  : Entity_Reference) return Boolean
   is
      R : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         return False;
      else
         R.Fetch
           (Xref.DB,
            SQL_Select
              (Database.Reference_Kinds.Is_Dispatching,
               From => Database.Reference_Kinds,
               Where => Database.Reference_Kinds.Id = "" & Ref.Kind_Id));
         return Boolean_Value (R, 0);
      end if;
   end Is_Dispatching_Call;

   -----------------
   -- Internal_Id --
   -----------------

   function Internal_Id (Entity : Entity_Information) return Integer is
   begin
      return Entity.Id;
   end Internal_Id;

   ----------------------
   -- From_Internal_Id --
   ----------------------

   function From_Internal_Id (Id : Integer) return Entity_Information is
   begin
      return (Id => Id, Fuzzy => False);
   end From_Internal_Id;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date
     (Self : Xref_Database; File : GNATCOLL.VFS.Virtual_File) return Boolean
   is
   begin
      if File = No_File then
         --  A predefined entity
         return True;

      else
         declare
            N : aliased String :=
              +File.Unix_Style_Full_Name (Normalize => True);
            Files    : Forward_Cursor;
         begin
            --  For efficiency, we do not store the timestamps for source files
            --  in the database. So we need to compare the timestamp of the LI
            --  file from the database with the source stamp

            Files.Fetch
              (Self.DB, Query_Get_ALI,
               Params => (1 => +N'Unchecked_Access));

            if Files.Has_Row then
               if Active (Me_Debug) then
                  Assert (Me_Debug,
                          not Files.Is_Null (1),
                          "No registered timestamp for ALI file "
                          & Files.Value (0));
                  Trace (Me_Debug,
                         "ali db=" & Image (Files.Time_Value (1), "%D-%T")
                         & " src disk="
                         & Image (File.File_Time_Stamp, "%D-%T"));
               end if;
               return Files.Time_Value (1) >= File.File_Time_Stamp;
            else
               return False;  --  file not even known in database
            end if;
         end;
      end if;
   end Is_Up_To_Date;

   ------------------------------
   -- All_Real_Reference_Kinds --
   ------------------------------

   function All_Real_Reference_Kinds
     (Xref : Xref_Database) return GNAT.Strings.String_List
   is
      R : Direct_Cursor;
   begin
      R.Fetch
        (Xref.DB,
         SQL_Select
           (Reference_Kinds.Display,
            From     => Reference_Kinds,
            Where    => Reference_Kinds.Is_Real = True,
            Order_By => Reference_Kinds.Display));

      return Result : String_List (1 .. Integer (R.Rows_Count)) do
         for Index in Result'Range loop
            Result (Index) := new String'(Value (R, 0));
            R.Next;
         end loop;
      end return;
   end All_Real_Reference_Kinds;

   --------------------
   -- From_Instances --
   --------------------

   function From_Instances
     (Self   : Xref_Database'Class;
      Ref    : Entity_Reference) return Entity_Array
   is
      Inst : Entity_Information;
      R    : Forward_Cursor;
   begin
      if Ref.Kind_Id = Kind_Id_Declaration then
         Inst := Self.Instance_Of (Ref.Entity);
         if Inst = No_Entity then
            --  No instantiation
            return (1 .. 0 => No_Entity);
         else
            --  Instantiation of a single entity.
            return (1 .. 1 => Inst);
         end if;

      else
         R.Fetch
           (Self.DB,
            SQL_Select
              (Entity_Refs.From_Instantiation,
               From => Entity_Refs & Files,
               Where => Entity_Refs.Entity = Ref.Entity.Id
               and Entity_Refs.Line = Ref.Line
               and Entity_Refs.Column = Integer (Ref.Column)
               and Entity_Refs.File = Files.Id
               and Files.Path = (+Ref.File.Unix_Style_Full_Name
                 (Normalize => True))));

         if R.Has_Row then
            declare
               Str  : GNAT.Strings.String_List_Access :=
                 Split (R.Value (0), ',');
               Result : Entity_Array (Str'Range);
            begin
               --  Need to search which entities each of the location
               --  corresponds to.

               for S in Str'Range loop
                  declare
                     Pipe : constant Integer :=
                       Ada.Strings.Fixed.Index (Str (S).all, "|");

                     File : constant Integer :=
                       Integer'Value (Str (S)(Str (S)'First .. Pipe - 1));

                     Line : constant Integer :=
                       Integer'Value (Str (S)(Pipe + 1 .. Str (S)'Last));
                  begin
                     R.Fetch
                       (Self.DB,
                        SQL_Select
                          (Entities.Id,
                           From => Entities,
                           Where => Entities.Decl_File = File
                           and Entities.Decl_Line = Line));

                     if R.Has_Row then
                        Result (S) :=
                          (Id => R.Integer_Value (0),
                           Fuzzy => False);
                     else
                        Result (S) := No_Entity;
                     end if;
                  end;
               end loop;
               Free (Str);

               return Result;
            end;

         else
            --  No instantiation
            return (1 .. 0 => No_Entity);
         end if;
      end if;
   end From_Instances;

   -----------------
   -- From_Prefix --
   -----------------

   procedure From_Prefix
     (Self       : Xref_Database'Class;
      Prefix     : String;
      Is_Partial : Boolean := True;
      Cursor     : out Entities_Cursor'Class)
   is
      Q : SQL_Query;
   begin
      if Is_Partial then
         Q := SQL_Select
           (Entities2_Fields,
            From      => Entities2,
            Where    => Like (Entities2.Name, Prefix & '%'),
            Order_By => Entities2.Name);
      else
         Q := SQL_Select
           (Entities2_Fields,
            From      => Entities2,
            Where    => Entities2.Name = Prefix,
            Order_By => Entities2.Name);
      end if;

      Cursor.DBCursor.Fetch (Self.DB, Q);
   end From_Prefix;

   ----------------
   -- Add_Entity --
   ----------------

   function Add_Entity
     (Self         : Xref_Database;
      Name         : String;
      Kind         : String;
      Decl_File    : GNATCOLL.VFS.Virtual_File;
      Decl_Project : Project_Type;
      Decl_Line    : Natural;
      Decl_Column  : Natural) return Entity_Information
   is
      N : aliased String :=
        +Decl_File.Unix_Style_Full_Name (Normalize => True);
      Project_N : aliased String :=
        +Decl_Project.Project_Path.Unix_Style_Full_Name (Normalize => True);
      K : aliased String := Kind;
      Na : aliased String := Name;
      Id          : Integer;
      Was_Started : Boolean;
   begin
      Was_Started := Self.DB.Start_Transaction;

      Id := Self.DB.Insert_And_Get_PK
        (SQL_Insert
           (Fields => Entities.Name & Entities.Kind & Entities.Decl_File
            & Entities.Decl_Line & Entities.Decl_Column,
            Values => SQL_Select
              (Text_Param (3)
               & Entity_Kinds.Id
               & Files.Id
               & Integer_Param (4) & Integer_Param (5),
               From => Entity_Kinds & Files & Files3,
               Where => Entity_Kinds.Display = Text_Param (2)
               and Files.Path = Text_Param (1)
               and Files.Project = Files3.Id
               and Files3.Path = Text_Param (6))),
         Params =>
           (1 => +N'Unchecked_Access,
            2 => +K'Unchecked_Access,
            3 => +Na'Unchecked_Access,
            4 => +Decl_Line,
            5 => +Decl_Column,
            6 => +Project_N'Unchecked_Access),
         PK => Entities.Id);

      if Was_Started then
         Self.DB.Commit;
      end if;

      return Entity_Information'
        (Fuzzy => False,
         Id    => Id);
   exception
      when E : others =>
         Trace (Me_Error, E);
         if Was_Started then
            Self.DB.Rollback;
         end if;
         return No_Entity;
   end Add_Entity;

   ------------------
   -- Needs_Update --
   ------------------

   procedure Needs_Update
     (Self                : in out Xref_Database;
      Project             : Project_Type;
      Update_Needed       : out Boolean;
      Parse_Runtime_Files : Boolean := True)

   is
      LI_Files       : Library_Info_List;
      LIs            : LI_Lists.List;
      Absolute_Start : Time;
   begin
      if Active (Me_Timing) then
         Absolute_Start := Clock;
      end if;

      Update_Needed := True;

      Project.Library_Files
        (Recursive => True, Xrefs_Dirs => True, Including_Libraries => True,
         ALI_Ext => "^.*\.[ags]li$", List => LI_Files,
         Include_Predefined => Parse_Runtime_Files);

      if Active (Me_Timing) then
         Trace (Me_Timing,
                "Found" & Length (LI_Files)'Img & " [ags]li files");
      end if;

      declare
         DB_Created : Boolean;
      begin
         Initialize_DB (Self, Self.DB, "", DB_Created,
                        Force => False);  --  From_DB_Name -> Self.DB
      end;

      if Self.DB_Created then
         Search_LI_Files_To_Update
           (Self, LI_Files, LIs, Force_Refresh => False);
         Update_Needed := not LIs.Is_Empty;
      else
         Update_Needed := not LI_Files.Is_Empty;
      end if;

      if Active (Me_Timing) then
         Trace (Me_Timing, "Total time:"
                & Duration'Image (Clock - Absolute_Start) & " s");
      end if;

   end Needs_Update;

end GNATCOLL.Xref;
