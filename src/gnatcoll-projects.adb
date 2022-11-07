------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2022, AdaCore                     --
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
with Ada.Calendar;                use Ada.Calendar;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Generic_Array_Sort;
with Ada.Directories;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;

with System;                      use System;

with GNAT.Case_Util;
with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.Expect;                 use GNAT.Expect;
with GNAT.Expect.TTY;             use GNAT.Expect.TTY;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with GNATCOLL.Projects.Normalize; use GNATCOLL.Projects.Normalize;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;          use GNATCOLL.VFS_Utils;

with GNATCOLL.Projects.Krunch;

with GPR.Util;                    use GPR.Util;
with GPR.Osint;
with GPR.Opt;
with GPR.Output;
with GPR.Attr;                    use GPR.Attr;
with GPR.Com;
with GPR.Conf;                    use GPR.Conf;
with GPR.Env;                     use GPR, GPR.Env;
with GPR.Err;
with GPR.Ext;
with GPR.Names;                   use GPR.Names;
with GPR.Part;
with GPR.Proc;
with GPR.PP;                      use GPR.PP;
with GPR.Tree;                    use GPR.Tree;
with GPR.Sinput;
with GPR.Snames;                  use GPR.Snames;
with GPR.Knowledge;
with GPR.Sdefault;

with DOM.Core.Nodes;
with DOM.Core.Documents;
with Input_Sources.File;
with Sax.Readers;
with Schema.Dom_Readers;

package body GNATCOLL.Projects is

   package GU renames GNATCOLL.Utils;

   Me    : constant Trace_Handle := Create ("Projects", Default => Off);
   Debug : constant Trace_Handle := Create ("Projects.Debug", Default => Off);
   Me_Gnat : constant Trace_Handle :=
     Create ("Projects.GNAT", GNATCOLL.Traces.Off);
   Me_Aggregate_Support : constant Trace_Handle :=
     Create ("Projects.Aggregate", Default => On);

   Me_SV : constant Trace_Handle := Create ("Projects.SV", Default => Off);
   --  Trace specific to Scenario/Untyped Variable computation. May create
   --  lots of output on relatively complex project trees, so makes sense
   --  to separate it from the main trace.

   Dummy_Suffix : constant String := "<no suffix defined>";
   --  A dummy suffixes that is used for languages that have either no spec or
   --  no implementation suffix defined.

   Unknown_Importing_Projects : aliased constant Path_Name_Id_Array (1 .. 0) :=
     (others => <>);
   --  A dummy array used while computing importing projects

   package Path_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use Path_Sets;

   package Virtual_File_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Virtual_File);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Scenario_Variable_Array, Scenario_Variable_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Untyped_Variable_Array, Untyped_Variable_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Type'Class, Project_Type_Access);

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (Arr : in out Project_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Project_Array, Project_Array_Access);
   begin
      Internal (Arr);
   end Unchecked_Free;

   package Project_Htables is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Virtual_File,   --  project path
      Element_Type    => Project_Type,
      Hash            => Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");
   --  maps project paths (casing insensitive) to project types
   --  ??? This would not be needed if we could, in the prj* sources, associate
   --  user data with project nodes.

   type Source_File_Data;
   type Source_File_Data_Access is access all Source_File_Data;
   type Source_File_Data is record
      Project : Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Lang    : GPR.Name_Id;
      Source  : GPR.Source_Id;
      Next    : Source_File_Data_Access := null;
   end record;
   --  In some case, Lang might be set to Unknown_Language, if the file was
   --  set in the project (for instance through the Source_Files attribute),
   --  but no matching language was found.
   --  Next is only relevant when there may be more than one source with same
   --  base name. This can happen when root project is aggregate project,
   --  languages other than Ada are involved (i.e. C), or when list of
   --  languages is changed on the fly and same sources can be treated as those
   --  of different languages. In that case we can have multiple source files
   --  with same base name but different full names. Even sources with same
   --  full name can belong to different aggregated projects, so there are
   --  different Source_File_Data instances for such each such project;

   function Hash
     (File : GNATCOLL.VFS.Filesystem_String) return Ada.Containers.Hash_Type;
   function Equal (F1, F2 : GNATCOLL.VFS.Filesystem_String) return Boolean;
   pragma Inline (Hash, Equal);
   --  Either case sensitive or not, depending on the system

   package Names_Files is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Filesystem_String,   --  file base name
      Element_Type    => Source_File_Data,
      Hash            => Hash,
      Equivalent_Keys => Equal);
   --  maps for file base names to info about the file

   procedure Include_File
     (Map  : in out Names_Files.Map;
      Key  : GNATCOLL.VFS.Filesystem_String;
      Elem : Source_File_Data);
   --  If there is no file with same base name in map, adds the file as the
   --  first element in corresponding list. Otherwise adds it to the list.

   procedure Clean_Up (Map : in out Names_Files.Map);
   --  Clean up possibly existing lists of files with similar base names,
   --  then Clear the map itself.

   function Hash (Node : Project_Node_Id) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);

   package Project_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Project_Node_Id,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Directory_Dependency is (Direct, As_Parent);
   --  The way a directory belongs to the project: either as a direct
   --  dependency, or because one of its subdirs belong to the project, or
   --  doesn't belong at all.

   package Directory_Statuses is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Filesystem_String,   --  Directory path
      Element_Type    => Directory_Dependency,
      Hash            => Hash,
      Equivalent_Keys => Equal);
   --  Whether a directory belongs to a project

   use Project_Htables, Extensions_Languages, Names_Files;
   use Directory_Statuses;

   type Project_Tree_Data (Is_Aggregated : Boolean) is record
      Env       : Project_Environment_Access;

      Tree      : GPR.Project_Node_Tree_Ref;
      View      : GPR.Project_Tree_Ref;
      --  The description of the trees

      Status  : Project_Status := From_File;

      Root    : Project_Type := No_Project;
      --  The root of the project hierarchy

      Directories : Directory_Statuses.Map;
      --  Index on directory name
      --  ??? might not be needed anymore, using the hash tables already
      --  in GPR.*

      Timestamp : Ada.Calendar.Time := GNATCOLL.Utils.No_Time;
      --  Time when we last parsed the project from the disk

      case Is_Aggregated is
         when False =>
            Sources : Names_Files.Map;
            --  Index on base source file names, returns information about
            --  the file.

            Objects_Basename : Names_Files.Map;
            --  The basename (with no extension or directory) of the object
            --  files.  This is used to quickly filter out the relevant object
            --  or library files when an object directory is shared amongst
            --  multiple projects.  This table does not point to the actual
            --  location of the object files, which might be in an extending
            --  project. It only provides a quick way to filter out irrelevant
            --  object files.

            Projects : Project_Htables.Map;
            --  Index on project paths. This table is filled when the project
            --  is loaded.

         when True =>
            null;
      end case;
   end record;

   procedure Free (Self : in out Project_Tree_Data_Access);
   --  Free memory used by Self.

   function Get_View
     (Tree : GPR.Project_Tree_Ref;
      Path : Path_Name_Type) return GPR.Project_Id;
   --  Return the project view for the project Name

   type External_Variable_Callback is access procedure
     (Variable : Project_Node_Id;
      Prj      : Project_Node_Id;
      Pkg      : Project_Node_Id;
      Project  : Project_Type);
   --  Called for a typed variable declaration that references an external
   --  variable in GPR.

   type Get_Directory_Path_Callback is access function
     (Project : GPR.Project_Id) return Path_Information;
   --  Called to get the directory path the Get_Directory function must
   --  return the Virtual_File

   function Get_Directory
    (Project  : Project_Type;
     Callback : Get_Directory_Path_Callback) return Virtual_File;
   --  return the Virtual_File generated from the callback return.
   --  If callback returns a 0 length Path_Information then function returns
   --  the project object directory.
   --  If project not accessible return No_File.

   function Variable_Value_To_List
     (Project : Project_Type;
      Value   : Variable_Value) return GNAT.Strings.String_List_Access;
   --  Allocate a new String_List that contains the strings stored in Value.
   --  Result must be freed by caller.

   procedure For_Each_Project_Node
     (Tree     : GPR.Project_Node_Tree_Ref;
      Root     : Project_Node_Id;
      Callback : access procedure
        (Tree : GPR.Project_Node_Tree_Ref; Node : Project_Node_Id));
   --  Iterate over all projects in the tree.
   --  They are each returned once, the root project first and then all its
   --  imported projects.
   --  As opposed to For_Every_Project_Imported, this iteration in based on the
   --  project tree, and therefore can be used before the project view has been
   --  computed.
   --  This includes projects extended by Root.
   --  The order is:
   --      root project, project, extended_project_of_project,...

   function Default_Spec_Suffix
     (Self          : Project_Environment'Class;
      Language_Name : String) return String;
   function Default_Body_Suffix
     (Self          : Project_Environment'Class;
      Language_Name : String) return String;
   --  Return the default extensions for a given language, as registered
   --  through Register_Default_Language_Extension;

   procedure For_Each_External_Variable_Declaration
     (Root_Project : Project_Type;
      Recursive    : Boolean;
      Callback     : External_Variable_Callback);
   --  Iterate other all the typed variable declarations that reference
   --  external variables in Project (or one of its imported projects if
   --  Recursive is true).
   --  Callback is called for each of them.

   procedure Append
     (Self : in out Path_Name_Array;
      Path : GPR.Path_Name_Type);
   --  Resize Self if needed, and append a new value

   procedure Reset
     (Tree : in out Project_Tree'Class;
      Env  : Project_Environment_Access);
   --  Make sure the Tree data has been created and initialized

   function Substitute_Dot
     (Unit_Name : String; Dot_Replacement : String) return String;
   --  Replace the '.' in unit_name with Dot_Replacement

   procedure Compute_Importing_Projects
     (Project      : Project_Type'Class;
      Root_Project : Project_Type'Class);
   --  Compute the list of all projects that import, possibly indirectly,
   --  Project.

   procedure Reset_View (Tree : Project_Tree'Class);
   --  Clear internal tables for the view

   function String_Elements
     (Data : Project_Tree_Data_Access)
      return GPR.String_Element_Table.Table_Ptr;
   pragma Inline (String_Elements);
   --  Return access to the various tables that contain information about the
   --  project

   function Get_String (Id : GPR.File_Name_Type) return String;
   function Get_String (Id : GPR.Path_Name_Type) return String;
   pragma Inline (Get_String);
   --  Return the string in Name
   --  Same as GPR.Get_Name_String, but return "" in case of
   --  failure, instead of raising Assert_Failure.

   function Create_Flags
     (On_Error        : GPR.Error_Handler;
      Require_Sources : Boolean := True;
      Ignore_Missing_With : Boolean := False;
      Report_Missing_Dirs : Boolean := True) return Processing_Flags;
   --  Return the flags to pass to the project manager in the context of GPS.
   --  Require_Sources indicates whether each language must have sources
   --  attached to it.

   function Length
     (Tree : GPR.Project_Tree_Ref; List : GPR.String_List_Id) return Natural;
   --  Return the number of elements in the list

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : String;
      Index        : String := "";
      Use_Extended : Boolean := False) return Variable_Value;
   --  Internal version of Attribute_Value

   function Has_Attribute
     (Project   : Project_Type;
      Attribute : String;
      Index     : String := "") return Boolean;
   --  Internal version of Has_Attribute

   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : String;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List;
   --  Internal version of Attribute_Indexes

   procedure Reset_View (Self : in out Project_Data'Class);
   --  Reset and free the internal data of the project view

   procedure Compute_Scenario_Variables
     (Tree      : Project_Tree_Data_Access;
      Recursive : Boolean := True;
      Errors    : Error_Report := null);
   --  Compute (and cache) the whole list of scenario variables for the
   --  project tree.
   --  This also ensures that each external reference actually exists

   function Source_File_Data_To_Info (S : Source_File_Data) return File_Info;
   --  Converts from one structure to the other

   procedure Compute_Imported_Projects (Project : Project_Type'Class);
   --  Compute and cache the list of projects imported by Project.
   --  Nothing is done if this is already known.
   --  This also include projects extended by Project.
   --  The order is
   --       root_project, project, project_extended_by_project, ...

   function Delete_File_Suffix
     (Filename : GNATCOLL.VFS.Filesystem_String;
      Project  : Project_Type) return Natural;
   --  Return the last index in Filename before the beginning of the file
   --  suffix. Suffixes are searched independently from the language.
   --  If not matching suffix is found in project, the returned value will
   --  simply be Filename'Last.

   procedure Internal_Load
     (Tree                   : in out Project_Tree'Class;
      Root_Project_Path      : GNATCOLL.VFS.Virtual_File;
      Errors                 : Projects.Error_Report;
      Report_Syntax_Errors   : Boolean;
      Project                : out Project_Node_Id;
      Packages_To_Check      : GNAT.Strings.String_List_Access := All_Packs;
      Recompute_View         : Boolean := True;
      Test_With_Missing_With : Boolean := True;
      Report_Missing_Dirs    : Boolean := True;
      Implicit_Project       : Boolean);
   --  Internal implementation of load. This doesn't reset the tree at all,
   --  but will properly setup the GNAT project manager so that error messages
   --  are redirected and fatal errors do not kill GPS.
   --  If Test_With_Missing_With is True, first test with ignoring unresolved
   --  "with" statement, in case we need to first parse the gnatlist attribute.

   procedure Parse_Source_Files (Self : in out Project_Tree);
   --  Find all the source files for the project, and cache them.
   --  At the same time, check that the gnatls attribute is coherent between
   --  all projects and subprojects, and memorize the sources in the
   --  hash-table.

   function Info
     (Tree : Project_Tree_Data_Access;
      File : GNATCOLL.VFS.Virtual_File) return File_Info;
   --  Internal version of Info

   procedure Create_Project_Instances
     (Self         : Project_Tree'Class;
      Tree_For_Map : Project_Tree'Class;
      With_View    : Boolean);
   function Instance_From_Node
     (Self         : Project_Tree'Class;
      Tree_For_Map : Project_Tree'Class;
      Node         : Project_Node_Id) return Project_Type;
   --  Create all instances of Project_Type for the loaded projects.
   --  Instances are put in Htable of Tree_For_Map parameter.
   --  This also resets the internal data for the view.

   function Handle_Subdir
     (Project : Project_Type;
      Id      : GPR.Path_Name_Type;
      Xref_Dirs : Boolean) return Filesystem_String;
   --  Adds the object subdirectory to Id if one is defined

   function Kind_To_Part (Source : Source_Id) return Unit_Parts;
   --  Converts from Source.Kind to Unit_Parts

   function Set_Path_From_Gnatls_Attribute
     (Project      : Project_Id;
      Tree         : Project_Tree'Class;
      Errors       : Error_Report := null)
      return Boolean;
   --  Look at the gnatls attribute, if defined, and update the predefined
   --  path if needed.
   --  Return True if the path was updated.

   procedure Put
      (Self        : in out Pretty_Printer'Class;
       Project     : Project_Node_Id;
       In_Tree     : GPR.Project_Node_Tree_Ref;
       Id          : Project_Id := GPR.No_Project;
       Increment   : Positive := 3;
       Eliminate_Empty_Case_Constructions : Boolean := False);
   --  Internal version of Put, acting directly on the low-level structures

   Specific_Attributes_Registered : Boolean := False;
   procedure Register_Specific_Attributes;
   --  Register specific attributes like IDE'Artifact_Dir but only once per
   --  program run. Second attempt at registering the attribute leads to an
   --  error in libgpr.

   Host_Targets_List     : GPR.Knowledge.String_Lists.List :=
     GPR.Knowledge.String_Lists.Empty_List;
   Host_Targets_List_Set : Boolean := False;
   procedure Set_Host_Targets_List;
   --  Populates the list of host targets that include the host target itself
   --  and may as well have corresponding fallback targets. It also parses
   --  targetset.xml and populates Normalisation_Dictionary (see below).

   type Targetset_Info is record
      Canonical_Name : Ada.Strings.Unbounded.Unbounded_String;
      Regexp_Imgs    : GPR.Knowledge.String_Lists.List;
   end record;

   function "<" (L, R : Targetset_Info) return Boolean is
     (Ada.Strings.Unbounded."<" (L.Canonical_Name, R.Canonical_Name));

   package Targetset_Info_Set is new
     Ada.Containers.Ordered_Sets (Targetset_Info);
   Normalization_Dictionary : Targetset_Info_Set.Set;

   function Normalize_Target_Name (Target_Name : String) return String;
   --  Normalizes name of target against Normalization_Dictionary. If no match
   --  is found return Target_Name as is.

   function To_Mixed (S : String) return String;

   package Language_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (String);
   use Language_Sets;

   procedure Languages
     (Project   : Project_Type;
      Recursive : Boolean := False;
      Langs     : in out Language_Sets.Set);
   --  Internal implementation of Languages for non-aggregate project.
   --  It will update Langs with the supported languages by Project.
   --  If Recursive is True, then it will also add the supported languages
   --  by its imported projects.

   -----------
   -- Lists --
   -----------

   type String_List_Iterator is record
      Current : Project_Node_Id;
      --  pointer to N_Literal_String or N_Expression
   end record;

   function Done (Iter : String_List_Iterator) return Boolean;
   --  Return True if Iter is past the end of the list of strings

   function Next
     (Tree : GPR.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator;
   --  Return the next item in the list

   function Data
     (Tree : GPR.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return GPR.Name_Id;
   --  Return the value pointed to by Iter.
   --  This could be either a N_String_Literal or a N_Expression node in the
   --  first case.
   --  The second case only works if Iter points to N_String_Literal.

   function Value_Of
     (Tree : GPR.Project_Node_Tree_Ref;
      Var  : Scenario_Variable) return String_List_Iterator;
   --  Return an iterator over the possible values of the variable

   ------------
   -- Errors --
   ------------

   generic
      Tree : Project_Tree'Class;
   procedure Mark_Project_Error (Project : Project_Id; Is_Warning : Boolean);
   --  Handler called when the project parser finds an error.
   --  Mark_Project_Incomplete should be true if any error should prevent the
   --  edition of project properties graphically.

   ------------------
   -- Kind_To_Part --
   ------------------

   function Kind_To_Part (Source : Source_Id) return Unit_Parts is
   begin
      if Source = null then
         return Unit_Separate;
      end if;

      case Source.Kind is
         when Spec => return Unit_Spec;
         when Impl => return Unit_Body;
         when Sep  => return Unit_Separate;
      end case;
   end Kind_To_Part;

   ------------------------
   -- Mark_Project_Error --
   ------------------------

   procedure Mark_Project_Error (Project : Project_Id; Is_Warning : Boolean) is
      P : Project_Type;
      pragma Warnings (Off, P);
      --  ??? Without the pragma Warnings (Off), when compiling with -gnatwae,
      --  we get this error:
      --    warning: variable "P" is assigned but never read
   begin
      if not Is_Warning then
         if Project = GPR.No_Project then
            if Tree.Root_Project /= No_Project then
               declare
                  Iter : Inner_Project_Iterator := Start (Tree.Root_Project);
               begin
                  while Current (Iter) /= No_Project loop
                     Current (Iter).Data.View_Is_Complete := False;
                     Next (Iter);
                  end loop;
               end;
            end if;

         else
            if Tree.Data.Root /= No_Project and then P.Data /= null then
               P := Project_Type (Project_From_Name (Tree.Data, Project.Name));
               P.Data.View_Is_Complete := False;
            end if;
         end if;
      end if;
   end Mark_Project_Error;

   ---------------
   -- Tree_View --
   ---------------

   function Tree_View (P : Project_Type'Class) return GPR.Project_Tree_Ref is
   begin
      return P.Data.Tree.View;
   end Tree_View;

   ---------------
   -- Tree_Tree --
   ---------------

   function Tree_Tree
     (P : Project_Type'Class) return GPR.Project_Node_Tree_Ref
   is
   begin
      return P.Data.Tree.Tree;
   end Tree_Tree;

   ---------------------
   -- String_Elements --
   ---------------------

   function String_Elements
     (Data : Project_Tree_Data_Access)
      return GPR.String_Element_Table.Table_Ptr is
   begin
      return Data.View.Shared.String_Elements.Table;
   end String_Elements;

   ------------
   -- Length --
   ------------

   function Length
     (Tree : GPR.Project_Tree_Ref; List : GPR.String_List_Id) return Natural
   is
      L     : String_List_Id := List;
      Count : Natural        := 0;
   begin
      while L /= Nil_String loop
         Count := Count + 1;
         L := Tree.Shared.String_Elements.Table (L).Next;
      end loop;
      return Count;
   end Length;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : GPR.File_Name_Type) return String is
   begin
      if Id = GPR.No_File then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end Get_String;

   function Get_String (Id : GPR.Path_Name_Type) return String is
   begin
      if Id = GPR.No_Path then
         return "";
      end if;

      return Get_Name_String (Id);

   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end Get_String;

   ----------
   -- Name --
   ----------

   function Name (Project : Project_Type) return String is
   begin
      if Project.Data = null then
         return "default";

      elsif Get_View (Project) /= GPR.No_Project then
         return Get_String (Get_View (Project).Display_Name);

      else
         return Get_String
           (GPR.Tree.Name_Of (Project.Data.Node, Project.Tree_Tree));
      end if;
   end Name;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path
     (Project : Project_Type;
      Host    : String := Local_Host) return GNATCOLL.VFS.Virtual_File
   is
      View : constant GPR.Project_Id := Get_View (Project);
   begin
      if
        Project.Data = null
        or else Project.Data.Node = Empty_Project_Node
      then
         return GNATCOLL.VFS.No_File;

      elsif View = GPR.No_Project then
         --  View=GPR.No_Project case needed for the project wizard
         return To_Remote
           (Create (+Get_String (Path_Name_Of
            (Project.Data.Node, Project.Tree_Tree))),
            Host);

      else
         return To_Remote
           (Create (+Get_String (View.Path.Display_Name)), Host);
      end if;
   end Project_Path;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs
     (Project                  : Project_Type;
      Recursive                : Boolean := False;
      Include_Externally_Built : Boolean := True)
      return GNATCOLL.VFS.File_Array
   is
      Current_Dir : constant Filesystem_String := Get_Current_Dir;
      Iter        : Inner_Project_Iterator;
      Count       : Natural := 0;
      P           : Project_Type;
      View        : Project_Id;
      Src         : String_List_Id;

      Aggregated      : Aggregated_Project_List;
      Aggregated_Dirs : File_Array_Access := null;

   begin
      if Is_Aggregate_Project (Project) and then Recursive then
         Aggregated := Project.Data.View.Aggregated_Projects;
         while Aggregated /= null loop
            Append
              (Aggregated_Dirs,
               Source_Dirs
                 (Project_From_Path
                    (Project.Data.Tree, Aggregated.Path),
                  Recursive => True,
                  Include_Externally_Built => Include_Externally_Built));
            Aggregated := Aggregated.Next;
         end loop;
         return Aggregated_Dirs.all;
      end if;

      Iter := Start (Project, Recursive);
      loop
         P := Current (Iter);
         exit when P = No_Project;

         View := Get_View (P);
         exit when View = GPR.No_Project;

         if Include_Externally_Built or else not Externally_Built (P) then
            Count := Count + Length (Project.Tree_View, View.Source_Dirs);
         end if;

         Next (Iter);
      end loop;

      declare
         Sources : File_Array (1 .. Count);
         Index   : Natural := Sources'First;
      begin
         Iter := Start (Project, Recursive);

         loop
            P := Current (Iter);
            exit when P = No_Project;

            View := Get_View (P);
            exit when View = GPR.No_Project;

            if Include_Externally_Built or else not Externally_Built (P) then
               Src := View.Source_Dirs;

               while Src /= Nil_String loop
                  Sources (Index) := Create
                    (Normalize_Pathname
                       (+Get_String
                          (String_Elements (P.Data.Tree) (Src).Display_Value),
                        Current_Dir,
                        Resolve_Links => False));
                  Ensure_Directory (Sources (Index));
                  Index := Index + 1;
                  Src   := String_Elements (P.Data.Tree) (Src).Next;
               end loop;
            end if;

            Next (Iter);
         end loop;

         return Sources (1 .. Index - 1);
      end;
   end Source_Dirs;

   -------------------
   -- Handle_Subdir --
   -------------------

   function Handle_Subdir
     (Project   : Project_Type;
      Id        : GPR.Path_Name_Type;
      Xref_Dirs : Boolean) return Filesystem_String
   is
      View : constant Project_Id := Get_View (Project);
      Env  : constant Project_Environment_Access := Project.Data.Tree.Env;
      Path : constant Filesystem_String :=
        Name_As_Directory (+Get_String (Id));
   begin
      if not Xref_Dirs
        or else Xrefs_Subdir (Env.all)'Length = 0
        or else View.Externally_Built
      then
         return Path;
      elsif GPR.Subdirs /= null then
         return Name_As_Directory
           (Path (Path'First .. Path'Last - GPR.Subdirs.all'Length - 1) &
            Xrefs_Subdir (Env.all));
      else
         return Path & Name_As_Directory (Xrefs_Subdir (Env.all));
      end if;
   end Handle_Subdir;

   ----------------
   -- Object_Dir --
   ----------------

   function Object_Dir
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      View : constant Project_Id := Get_View (Project);
   begin
      if View /= GPR.No_Project
        and then View.Object_Directory /= No_Path_Information
      then
         return
           Create
             (Handle_Subdir
                (Project, View.Object_Directory.Display_Name, False));
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Object_Dir;

   -------------------
   -- Artifacts_Dir --
   -------------------

   function Artifacts_Dir
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      D : GNATCOLL.VFS.Virtual_File;
      Att : constant Attribute_Pkg_String :=
        Build ("IDE", "Artifacts_Dir");
   begin
      if
        Project.Data.Tree.Env.IDE_Mode
        and then Project.Has_Attribute (Att)
        and then Attribute_Value (Project, Att) /= ""
      then
         D := Create_From_Base
           (+Attribute_Value (Project, Att),
            Project.Project_Path.Dir_Name);
         Ensure_Directory (D);
         return D;
      end if;
      if Project.Object_Dir /= GNATCOLL.VFS.No_File then
         return Project.Object_Dir;
      end if;

      Trace (Me, Project.Name & " does not have an object dir");
      D := Create
        (Project.Project_Path.Dir_Name & Project.Data.Tree.Env.Object_Subdir);
      Ensure_Directory (D);
      if Is_Writable (D) then
         return D;
      else
         Trace
           (Me, "Directory '" & D.Display_Full_Name & "' is not writable");
         return GNATCOLL.VFS.No_File;
      end if;
   end Artifacts_Dir;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := False;
      Xrefs_Dirs          : Boolean := False;
      Exclude_Externally  : Boolean := False) return File_Array
   is
      View : constant Project_Id := Get_View (Project);

   begin
      if View = GPR.No_Project then
         return (1 .. 0 => <>);

      elsif Recursive then
         declare
            Iter  : Project_Iterator := Start (Project, Recursive);
            Result : File_Array_Access;
            P     : Project_Type;
         begin
            loop
               P := Current (Iter);
               exit when P = No_Project or else P.Get_View = GPR.No_Project;

               Prepend (Result,
                        P.Object_Path
                          (Recursive           => False,
                           Including_Libraries => Including_Libraries,
                           Xrefs_Dirs          => Xrefs_Dirs,
                           Exclude_Externally  => Exclude_Externally));
               Next (Iter);
            end loop;

            return R : constant File_Array := Result.all do
               Unchecked_Free (Result);
            end return;
         end;

      elsif Including_Libraries
        and then View.Library
        and then View.Library_ALI_Dir /= No_Path_Information
      then
         --  Object_Directory is in fact always defined for projects read from
         --  files (if unspecified in the user's project, it defaults to the
         --  projects' own directory).
         --  For externally_built library projects, however, it should not be
         --  taken into account.

         if View.Externally_Built and then Exclude_Externally then
            return (1 .. 0 => <>);

         elsif View.Object_Directory = No_Path_Information
           or else View.Externally_Built
         then
            return (1 => Create
                    (Handle_Subdir
                       (Project, View.Library_ALI_Dir.Display_Name,
                        Xrefs_Dirs)));
         else
            return
              (Create (Handle_Subdir
                         (Project, View.Object_Directory.Display_Name,
                          Xrefs_Dirs)),
               Create (Handle_Subdir
                         (Project, View.Library_ALI_Dir.Display_Name,
                          Xrefs_Dirs)));
         end if;

      elsif View.Object_Directory /= No_Path_Information then
         return
           (1 => Create (Handle_Subdir
                           (Project, View.Object_Directory.Display_Name,
                            Xrefs_Dirs)));
      else
         return (1 .. 0 => <>);
      end if;
   end Object_Path;

   -------------------
   -- Library_Files --
   -------------------

   procedure Library_Files
     (Self                : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := True;
      Xrefs_Dirs          : Boolean := False;
      ALI_Ext             : GNATCOLL.VFS.Filesystem_String := ".ali";
      Include_Predefined  : Boolean := False;
      List                : in out Library_Info_List'Class;
      Exclude_Overridden  : Boolean := True)
   is
      Tmp             : File_Array_Access;
      Prj_Iter        : Project_Iterator;
      Current_Project : Project_Type;
      Info_Cursor     : Names_Files.Cursor;
      Re              : Pattern_Matcher_Access;

      package Virtual_File_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type        => Virtual_File,
         Hash                => Full_Name_Hash,
         Equivalent_Elements => "=",
         "="                 => "=");
      use Virtual_File_Sets;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Pattern_Matcher, Pattern_Matcher_Access);

      function Get_Base_Name (F : Virtual_File) return Filesystem_String;
      --  Return the base name of the argument. If ALI_Ext was a regular
      --  expression, this function simply strips the file extension
      --  (everything after and including the last dot in the file name).
      --  Otherwise, the suffix ALI_Ext is removed from the file name.

      function Is_Extending_All (P : Project_Type) return Boolean is
        (Is_Extending_All (P.Data.Node, P.Data.Tree.Tree));

      function Find_Unit_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type)
         return Names_Files.Cursor;
      function Find_File_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type)
         return Names_Files.Cursor;
      --  Searches for Source_File_Data with given base name and a project from
      --  a project subtree that starts from Root.
      --  If the resulting Source_File_Data is not the first one in the list,
      --  it is placed in Local_Obj_Map and returned Cursor points to it.
      --  Local_Obj_Map must be cleared after each object file is processed.
      --
      --  ??? This function seems to be the same as Create_From_Project.

      procedure Process_Project (Project : Project_Type);
      --  Process Project and append to List all relevant ALI files

      function Is_File_Based (SFD : Source_File_Data) return Boolean;
      --  Return whether the source file designated by SFD belongs to a
      --  file-based language (i.e. not a unit-based language, like Ada).

      Local_Obj_Map : Names_Files.Map;

      -------------------
      -- Get_Base_Name --
      -------------------

      function Get_Base_Name (F : Virtual_File) return Filesystem_String is
      begin
         if Re = null then
            return F.Base_Name (ALI_Ext);
         else
            return F.Base_Name (F.File_Extension);
         end if;
      end Get_Base_Name;

      --------------------------
      -- Find_Unit_In_Subtree --
      --------------------------

      function Find_Unit_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type) return Names_Files.Cursor
      is
         Cur   : Names_Files.Cursor;
         Iter  : Project_Iterator;
         SFD   : Source_File_Data;
      begin
         Cur := Map.Find (Key);
         if Cur = Names_Files.No_Element then
            --  No object files with same base name expected for any project.
            return Cur;
         end if;

         SFD := Element (Cur);

         loop
            if not Is_File_Based (SFD) then

               Iter := Start (Extending_Project (Root, True));
               while Current (Iter) /= No_Project loop
                  if Current (Iter) = SFD.Project then
                     --  Creating a temporary element to point to.

                     Local_Obj_Map.Include (Key, SFD);
                     return Local_Obj_Map.First;
                  end if;

                  Next (Iter);
               end loop;

            end if;

            exit when SFD.Next = null;

            SFD := SFD.Next.all;
         end loop;

         return Names_Files.No_Element;
      end Find_Unit_In_Subtree;

      --------------------------
      -- Find_File_In_Subtree --
      --------------------------

      function Find_File_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type) return Names_Files.Cursor
      is
         Cur   : Names_Files.Cursor;
         SFD   : Source_File_Data;
         Extended_P : Project_Type;
      begin
         Cur := Map.Find (Key);
         if Cur = Names_Files.No_Element then
            --  No object files with same base name expected for any project.
            return Cur;
         end if;

         SFD := Element (Cur);

         --  Use a standard iterator (and remove aggregated projects ourselves)
         --  instead of an inner iterator, so that library projects aggregated
         --  in a library aggregate are also considered
         loop
            if Is_File_Based (SFD) then

               --  We can have as much c/c++ files with same name as possible.
               --  So what we need to do is only iterate through extended
               --  projects to check whether the current file belongs to them,
               --  but not through the whole project subtree, since we can find
               --  absolutely unrelated homonyms.

               Extended_P := Extending_Project (Root, True);
               loop
                  --  All C library files are created when compiling a
                  --  <basename>.c or <basename>.cpp file, so when processing
                  --  a C library file, we want Library_Files to return
                  --  <basename>.c(pp) as the corresponding source for
                  --  <basename>.c.gli, and not the source of a homonym header.
                  --
                  --  Since all compiled .c(pp) files generate a corresponding
                  --  .c.gli file, we are guaranteed to find some
                  --  Source_File_Data corresponding to a .c(pp) file with the
                  --  same basename as Key, even if we skip the header files.

                  if Extended_P = SFD.Project
                    and then Kind_To_Part (SFD.Source) = Unit_Body
                  then
                     Local_Obj_Map.Include (Key, SFD);

                     return Local_Obj_Map.First;
                  end if;

                  Extended_P := Extended_Project (Extended_P);
                  exit when Extended_P = No_Project;
               end loop;

            end if;

            exit when SFD.Next = null;

            SFD := SFD.Next.all;
         end loop;

         return Names_Files.No_Element;
      end Find_File_In_Subtree;

      Seen, Added : Virtual_File_Sets.Set;

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : Project_Type) is
         Objects  : constant File_Array :=
           Object_Path (Project,
                        Recursive           => False,
                        Including_Libraries => Including_Libraries,
                        Xrefs_Dirs          => Xrefs_Dirs);
         Dir            : Virtual_File;
         Should_Append  : Boolean;
         Lowest_Project : Project_Type;

      begin
         if Objects'Length = 0
           or else Seen.Contains (Objects (Objects'First))
         then
            return;
         end if;

         --  Only look at the first object directory (which is either
         --  object_dir, if it exists, or library_dir, if it exists).
         --  We never need to look at both of them.

         Dir := Objects (Objects'First);
         Seen.Include (Dir);
         Trace (Me, "Library_Files, reading dir "
                & Dir.Display_Full_Name);
         Tmp := Read_Dir (Dir);

         for F in Tmp'Range loop
            if (Re /= null
                and then Match (Re.all, +Tmp (F).Base_Name))
              or else (Re = null
                       and then Tmp (F).Has_Suffix (ALI_Ext))
            then
               declare
                  B : constant Filesystem_String := Get_Base_Name (Tmp (F));

                  B_Last : Integer := B'Last;
                  Dot    : Integer;
                  P      : Project_Type;

               begin
                  Info_Cursor := Find_Unit_In_Subtree
                    (Self.Data.Tree_For_Map.Objects_Basename,
                     B, Self);

                  if not Has_Element (Info_Cursor) then
                     --  Special case for C files: the library file is
                     --    file.c.gli
                     --  instead of file.ali as we would have in Ada

                     Dot := B'Last;
                     while Dot >= B'First and then B (Dot) /= '.' loop
                        Dot := Dot - 1;
                     end loop;

                     if Dot > B'First then
                        B_Last := Dot - 1;
                        Info_Cursor :=
                          Find_File_In_Subtree
                            (Self.Data.Tree_For_Map.Objects_Basename,
                             B (B'First .. B_Last),
                             Project);
                     end if;
                  end if;

                  --  An LI file is taking into account if:
                  --  * it has a name that is known in this
                  --    project (and thus matches one source file).
                  --    This is a quick filter.
                  --  * AND it is not overridden in one of the
                  --    extending projects.
                  --    This test is not necessary if we don't want
                  --    to filter out overridden LI files

                  if not Has_Element (Info_Cursor) then
                     if Active (Me) then
                        Trace (Me, "Library_Files not including : "
                               & Display_Base_Name (Tmp (F))
                               & " (which is for unknown project)");
                     end if;

                     Should_Append := False;

                  elsif not Exclude_Overridden then
                     Lowest_Project := Element (Info_Cursor).Project;
                     Should_Append := Lowest_Project /= No_Project;

                  else
                     --  P is the candidate project that contains the
                     --  LI file, but the latter might be overridden
                     --  in any project extending P.

                     P := Element (Info_Cursor).Project;

                     --  This will contain the most-extending project
                     --  that contains a homonym of the LI file
                     Lowest_Project := P;

                     P := P.Extending_Project;

                     For_Each_Extending_Project :
                     while P /= No_Project loop
                        declare
                           Objs  : constant File_Array :=
                             P.Object_Path
                               (Recursive           => False,
                                Including_Libraries =>
                                  Including_Libraries,
                                Xrefs_Dirs          => Xrefs_Dirs);
                        begin
                           for Obj in Objs'Range loop
                              if Create_From_Base
                                (Tmp (F).Base_Name,
                                 Objs (Obj).Full_Name.all).Is_Regular_File
                              then
                                 if Active (Me) then
                                    Trace
                                      (Me, "overridden in project " & P.Name);
                                 end if;

                                 Lowest_Project := P;
                                 exit;
                              end if;
                           end loop;
                        end;

                        P := P.Extending_Project;
                     end loop For_Each_Extending_Project;

                     --  Since we are traversing each directory only once, we
                     --  cannot check that Lowest_Project is Project. Instead,
                     --  we need to check with the object dirs.

                     Should_Append := Lowest_Project = P;

                     if not Should_Append then
                        declare
                           Lowest_Objs : constant File_Array :=
                              Lowest_Project.Object_Path
                                 (Recursive => False,
                                  Including_Libraries =>
                                     Including_Libraries,
                                  Xrefs_Dirs          => Xrefs_Dirs);

                        begin
                           for Ob in Lowest_Objs'Range loop
                              Should_Append := Lowest_Objs (Ob) = Dir;
                              exit when Should_Append;
                           end loop;
                        end;
                     end if;
                  end if;
               end;

               --  Take into account Recursive parameter to decide
               --  whether the library file belongs to Self when
               --  Recursive = False, in case several projects share
               --  the same object directory. We can only do that if
               --  the project isn't extended though.

               if Should_Append
                 and then not Recursive
                 and then
                   Lowest_Project.Extending_Project = No_Project
               then
                  Should_Append := Lowest_Project = Self;
               end if;

               if Has_Element (Info_Cursor) and then
                 Added.Contains (Element (Info_Cursor).File)
               then
                  Trace (Me, "Library_Files not including : "
                         & Display_Base_Name (Tmp (F))
                         & " (which is overwritten in extends all project)");
               elsif Should_Append then
                  List.Append
                    (Library_Info'
                       (Library_File => Tmp (F),
                        LI_Project => new Project_Type'
                          (Lowest_Project),
                        Non_Aggregate_Root_Project =>
                           new Project_Type'(Self),
                        Source       => new File_Info'
                          (Source_File_Data_To_Info
                               (Element (Info_Cursor)))));
               elsif
                 Has_Element (Info_Cursor) and then
                 Is_Extending_All (Project)
               then
                  --  Corresponding source is not from current project, but
                  --  current project is an extends all project, so any
                  --  library file for a source of any project of the subtree
                  --  belongs to current project.

                  List.Append
                    (Library_Info'
                       (Library_File => Tmp (F),
                        LI_Project => new Project_Type'
                          (Lowest_Project),
                        Non_Aggregate_Root_Project =>
                           new Project_Type'(Self),
                        Source       => new File_Info'
                          (Source_File_Data_To_Info
                               (Element (Info_Cursor)))));

                  if Exclude_Overridden then
                     --  Also so that we do not include corresponding
                     --  overridden ALI file from the corresponding project,
                     --  we need to store its name explicitly.
                     Added.Include (Element (Info_Cursor).File);
                  end if;

               elsif Active (Me)
                 and then Has_Element (Info_Cursor)
               then
                  Trace (Me, "Library_Files not including : "
                         & Display_Base_Name (Tmp (F))
                         & " (which is for project "
                         & Element (Info_Cursor).Project.Name
                         & ")");
               end if;

               Local_Obj_Map.Clear;
            end if;
         end loop;

         Unchecked_Free (Tmp);

      exception
         when VFS_Directory_Error =>
            Trace (Me, "Couldn't open the directory "
                   & Dir.Display_Full_Name);
      end Process_Project;

      -------------------
      -- Is_File_Based --
      -------------------

      function Is_File_Based (SFD : Source_File_Data) return Boolean is
         Language : constant Language_Ptr :=
           Get_Language_From_Name
             (Get_View (SFD.Project), Get_String (SFD.Lang));
      begin
         return Language.Config.Kind = File_Based;
      end Is_File_Based;

   begin
      if Is_Aggregate_Project (Self) then
         if Active (Me) then
            Increase_Indent (Me, "Library file for an aggregate project");
         end if;

         declare
            Aggregated : Aggregated_Project_List;
            P : Project_Type;
         begin
            --  processing aggregated project hierarchies
            Aggregated := Self.Data.View.Aggregated_Projects;

            while Aggregated /= null loop
               P := Project_Type
                 (Project_From_Path (Self.Data.Tree, Aggregated.Path));

               Library_Files
                 (Self                => P,
                  Recursive           => Recursive,
                  Including_Libraries => Including_Libraries,
                  Xrefs_Dirs          => Xrefs_Dirs,
                  ALI_Ext             => ALI_Ext,
                  Include_Predefined  => False,
                  List                => List,
                  Exclude_Overridden  => Exclude_Overridden);

               Aggregated := Aggregated.Next;
            end loop;
         end;

         if Active (Me) then
            Decrease_Indent (Me, "Done Library file for aggregate project");
         end if;

         return;
      end if;

      if Active (Me) then
         Increase_Indent
           (Me, "Library_Files for project "
            & Self.Project_Path.Display_Full_Name);
      end if;

      --  An extended project logically does not contain any ALI file when in
      --  non recursive mode, so we simply do not look for them.

      if not Recursive and then Self.Extending_Project /= No_Project then
         return;
      end if;

      if ALI_Ext (ALI_Ext'First) = '^' then
         Re := new Pattern_Matcher'(Compile (+ALI_Ext));
      end if;

      --  We do not call Object_Path with Recursive=>True, but instead
      --  iterate explicitly on the projects so that we can control which of
      --  the object_dir or library_dir we want to use *for each project*.
      --
      --  We always look for projects recursively: when the user specified
      --  Recursive=>False, we still want to look at the extended projects
      --  of Self, so that all ALI files are associated with the lowest
      --  extending project. If the user specified Recursive=>False and
      --  Self is an extended project, we have already exited this procedure.
      --
      --  We are seeing extending projects before extended projects

      Prj_Iter := Self.Start_Reversed (Recursive => True);
      loop
         Current_Project := Current (Prj_Iter);
         exit when Current_Project = No_Project;

         --  Ignore projects that the user is not interested in (i.e. in
         --  non recursive mode, ignore all non-extended projects)

         if Recursive
           or else Current_Project = Self
           or else Current_Project.Extending_Project (Recurse => True) = Self
         then
            if Active (Me) then
               Trace (Me, "Current project: "
                      & Current_Project.Project_Path.Display_Full_Name);
            end if;

            Process_Project (Current_Project);
         end if;

         Next (Prj_Iter);
      end loop;

      if Include_Predefined then
         declare
            Predef : constant File_Array_Access :=
              Self.Data.Tree.Env.Predefined_Object_Path;
            Tmp : File_Array_Access;
         begin
            for P in Predef'Range loop
               if not Seen.Contains (Predef (P))
                  and then Predef (P).Is_Directory
               then
                  Seen.Include (Predef (P));
                  Tmp := Read_Dir (Predef (P));

                  for F in Tmp'Range loop
                     if (Re /= null
                         and then Match (Re.all, +Tmp (F).Base_Name))
                       or else (Re = null
                                and then Tmp (F).Has_Suffix (ALI_Ext))
                     then
                        List.Append
                          (Library_Info'
                             (Library_File => Tmp (F),
                              Non_Aggregate_Root_Project => null,
                              LI_Project   => null,
                              Source       => null));
                     end if;
                  end loop;

                  Unchecked_Free (Tmp);
               end if;
            end loop;
         end;
      end if;

      Unchecked_Free (Re);

      Added.Clear;

      if Active (Me) then
         Decrease_Indent (Me, "Done library files");
      end if;
   end Library_Files;

   -------------------
   -- Library_Files --
   -------------------

   function Library_Files
     (Self                : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := True;
      Xrefs_Dirs          : Boolean := False;
      ALI_Ext             : GNATCOLL.VFS.Filesystem_String := ".ali";
      Include_Predefined  : Boolean := False;
      Exclude_Overridden  : Boolean := True)
      return GNATCOLL.VFS.File_Array_Access
   is
      use Library_Info_Lists;
      List   : Library_Info_List;
      C      : Library_Info_Lists.Cursor;
      Result : File_Array_Access;
      Index  : Integer;
   begin
      Library_Files
        (Self,
         Recursive           => Recursive,
         Including_Libraries => Including_Libraries,
         Xrefs_Dirs          => Xrefs_Dirs,
         Include_Predefined  => Include_Predefined,
         ALI_Ext             => ALI_Ext,
         Exclude_Overridden  => Exclude_Overridden,
         List                => List);

      Result := new File_Array (1 .. Integer (Length (List)));
      Index  := Result'First;

      C := List.First;
      while Has_Element (C) loop
         Result (Index) := Element (C).Library_File;
         Index := Index + 1;
         Next (C);
      end loop;

      List.Clear;

      return Result;
   end Library_Files;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out File_Info_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Info, File_Info_Access);
   begin
      Unchecked_Free (Self);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Library_Info) is
   begin
      Free (Self.Source);
      Unchecked_Free (Self.LI_Project);
      Unchecked_Free (Self.Non_Aggregate_Root_Project);
   end Free;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up (Map : in out Names_Files.Map) is
      El     : Source_File_Data;
      Tmp_El : Source_File_Data_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Source_File_Data, Source_File_Data_Access);

   begin
      for C in Map.Iterate loop
         El := Names_Files.Element (C);
         while El.Next /= null loop
            Tmp_El := El.Next;
            El.Next := El.Next.Next;
            Unchecked_Free (Tmp_El);
         end loop;
      end loop;
      Map.Clear;
   end Clean_Up;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out Library_Info_List) is
      L : Library_Info;
      C : Library_Info_Lists.Cursor := Self.First;
   begin
      while Library_Info_Lists.Has_Element (C) loop
         L := Library_Info_Lists.Element (C);
         Free (L);
         Library_Info_Lists.Next (C);
      end loop;

      Library_Info_Lists.Clear (Library_Info_Lists.List (Self)); --  inherited
   end Clear;

   --------------------------
   -- Direct_Sources_Count --
   --------------------------

   function Direct_Sources_Count (Project : Project_Type) return Natural is
   begin
      --  ??? Should directly use the size of Source_Files, since this is now
      --  precomputed when the project is loaded
      if Get_View (Project) = GPR.No_Project then
         return 0;
      else
         return Project.Data.Files'Length;
      end if;
   end Direct_Sources_Count;

   ------------------------------
   -- Source_File_Data_To_Info --
   ------------------------------

   function Source_File_Data_To_Info (S : Source_File_Data) return File_Info is
      Unit : Name_Id := No_Name;
   begin
      if S.Source /= null and then S.Source.Unit /= null then
         Unit := S.Source.Unit.Name;
      end if;

      return File_Info'
        (Project      => S.Project,
         Root_Project => S.Project,
         File         => S.File,
         Part         => Kind_To_Part (S.Source),
         Name         => Unit,
         Lang         => S.Lang);
   end Source_File_Data_To_Info;

   -------------------------
   -- Create_From_Project --
   -------------------------

   function Create_From_Project
     (Self            : Project_Type'Class;
      Name            : GNATCOLL.VFS.Filesystem_String)
      return File_Info
   is
      function Find_From_Base_Name
        (Name : GNATCOLL.VFS.Filesystem_String) return File_Info;
      --  Find the File_Info from the given base name

      function Create_From_Full_Name (File : Virtual_File) return File_Info;
      --  Create File_Info from the given file

      -------------------------
      -- Find_From_Base_Name --
      -------------------------

      function Find_From_Base_Name
        (Name : GNATCOLL.VFS.Filesystem_String) return File_Info
      is
         Curs       : Names_Files.Cursor;
         File       : Virtual_File;
         Source     : Source_File_Data;
         Imports    : Boolean;
         Iter       : Project_Iterator;
      begin
         --  Amongst all the files with the right basename, search the one, if
         --  any, that is visible from Self.

         Curs := Self.Data.Tree_For_Map.Sources.Find (Name);
         if Has_Element (Curs) then
            --  Check amongst all possibilities which one is in Self or its
            --  imported projects.

            Source := Element (Curs);
            loop
               Imports := Source.Project = Project_Type (Self)
                 or else Source.Project = No_Project;
               --  predefined source file
               if not Imports then
                  Iter := Self.Start
                    (Recursive        => True,
                     Include_Extended => True);
                  loop
                     exit when Current (Iter) = No_Project;

                     if Current (Iter) = Source.Project then
                        Imports := True;
                        exit;
                     end if;

                     Next (Iter);
                  end loop;
               end if;

               if Imports then
                  return Source_File_Data_To_Info (Source);
               end if;

               exit when Source.Next = null;
               Source := Source.Next.all;
            end loop;
         end if;

         --  Search in the predefined source path

         if Self.Data.Tree.Env.Predefined_Source_Path /= null then
            File := Locate_Regular_File
              (Name, Self.Data.Tree.Env.Predefined_Source_Path.all);

            if File /= GNATCOLL.VFS.No_File then
               Include_File
                 (Self.Data.Tree_For_Map.Sources, Name,
                  Source_File_Data'
                    (Project  => No_Project,
                     File     => File,
                     Lang     => No_Name,
                     Source   => null,
                     Next     => null));

               return File_Info'
                 (File         => File,
                  Project      => No_Project,
                  Root_Project => Project_Type (Self),
                  Part         => Unit_Separate,
                  Name         => No_Name,
                  Lang         => No_Name);
            end if;
         end if;

         return
           (File         => GNATCOLL.VFS.No_File,
            Project      => No_Project,
            Root_Project => Project_Type (Self),
            Part         => Unit_Separate,
            Name         => GPR.No_Name,
            Lang         => GPR.No_Name);
      end Find_From_Base_Name;

      ---------------------------
      -- Create_From_Full_Name --
      ---------------------------

      function Create_From_Full_Name (File : Virtual_File) return File_Info is
         Result : File_Info;
      begin
         Result := Info (Tree => Self.Data.Tree, File => File);

         if Result.File = GNATCOLL.VFS.No_File then
            Result := File_Info'
              (File         => File,
               Project      => No_Project,
               Root_Project => Project_Type (Self),
               Part         => Unit_Separate,
               Name         => GPR.No_Name,
               Lang         => GPR.No_Name);
         end if;

         return Result;
      end Create_From_Full_Name;

      File   : Virtual_File;
      Result : File_Info;
   begin
      if Project_Type (Self) = No_Project then
         return File_Info'
           (File         => Create (Name),
            Project      => No_Project,
            Root_Project => Project_Type (Self),
            Part         => Unit_Separate,
            Name         => GPR.No_Name,
            Lang         => GPR.No_Name);
      end if;

      if Is_Absolute_Path (Name) then
         File := Create (Normalize_Pathname (Name, Resolve_Links => False));
         return Create_From_Full_Name (File);
      else
         --  This is not an absolute name: first check the cache

         if Self.Data.Base_Name_To_Full_Path = null then
            --  If it's the first time we need the cache, create it here
            Self.Data.Base_Name_To_Full_Path := new Basename_To_Info_Cache.Map;
         end if;

         if Self.Data.Base_Name_To_Full_Path.Contains (String (Name)) then
            return Create_From_Full_Name
              (Self.Data.Base_Name_To_Full_Path.Element (String (Name)));
         else
            --  Not found in cache: get the result
            Result := Find_From_Base_Name (Name);

            --  .. and add it to the cache
            Self.Data.Base_Name_To_Full_Path.Insert
              (String (Name), Result.File);

            return Result;
         end if;
      end if;
   end Create_From_Project;

   ------------
   -- Create --
   ------------

   function Create
     (Self            : Project_Tree;
      Name            : Filesystem_String;
      Project         : Project_Type'Class := No_Project;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True)
      return GNATCOLL.VFS.Virtual_File
   is
      File      : GNATCOLL.VFS.Virtual_File;
      Ambiguous : Boolean;
   begin
      Create
        (Self, Name, Project, Use_Source_Path, Use_Object_Path,
         Ambiguous, File);
      return File;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self            : Project_Tree;
      Name            : GNATCOLL.VFS.Filesystem_String;
      Project         : Project_Type'Class := No_Project;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True;
      Ambiguous       : out Boolean;
      File            : out GNATCOLL.VFS.Virtual_File;
      Predefined_Only : Boolean := False)
   is
      Tree_For_Map  : Project_Tree_Data_Access;
      --  The root tree

      Base          : constant Filesystem_String := Base_Name (Name);
      Project2      : Project_Type;
      Path          : Virtual_File := GNATCOLL.VFS.No_File;
      Iterator      : Project_Iterator;
      Info_Cursor   : Names_Files.Cursor;
      Source_Info   : Source_File_Data;
      In_Predefined : Boolean := False;
      Duplicate_Obj : Boolean := False;

      function Ambiguous_Base_Name
        (First_SFD : Source_File_Data) return Boolean;
      --  Return false if any of source files in given list has different full
      --  paths than First_SFD.

      function Ambiguous_Base_Name
        (First_SFD : Source_File_Data) return Boolean
      is
         Next_SFD : Source_File_Data_Access := First_SFD.Next;
      begin
         while Next_SFD /= null loop
            if Next_SFD.File /= First_SFD.File then
               return True;
            end if;
            Next_SFD := Next_SFD.Next;
         end loop;

         return False;
      end Ambiguous_Base_Name;

   begin
      Ambiguous := False;

      if Self.Data = null then
         --  No view computed, we do not even know the source dirs
         File := GNATCOLL.VFS.No_File;
         return;
      end if;

      Tree_For_Map := Self.Data.Root.Data.Tree_For_Map;

      if Is_Absolute_Path (Name) then
         File := Create (Normalize_Pathname (Name, Resolve_Links => False));
         return;
      end if;

      --  Is the file already in the cache ?
      --  This cache is automatically filled initially when the project is
      --  loaded, so we know that all source files of the project are in the
      --  cache and will be returned efficiently

      if not Predefined_Only
         and then Project.Data = null
         and then Use_Source_Path
      then
         Info_Cursor := Tree_For_Map.Sources.Find (Base);

         if Has_Element (Info_Cursor) then
            --  Multiple cases for ambiguity:
            --    1 - multiple possible full paths
            --    2 - same full path in multiple projects

            declare
               C : Source_File_Data renames Element (Info_Cursor);
            begin
               if Ambiguous_Base_Name (Element (Info_Cursor)) then
                  Ambiguous := True;
                  File := GNATCOLL.VFS.No_File;
                  return;
               end if;

               if C.Next /= null then
                  Ambiguous := True;
               end if;
            end;

            File := Element (Info_Cursor).File;
            return;
         end if;
      end if;

      --  When looking for a project file, check among those that are loaded.
      --  This means we might be looking outside of the source and obj dirs.

      if Equal (File_Extension (Name), Project_File_Extension) then
         if Project.Data /= null then
            Iterator := Project.Start (Recursive => False);
         else
            Iterator := Self.Root_Project.Start (Recursive => True);
         end if;
         loop
            Project2 := Current (Iterator);
            exit when Project2 = No_Project;

            if Case_Insensitive_Equal
              (+Project2.Project_Path.Base_Name, +Base)
            then
               if Path = GNATCOLL.VFS.No_File then
                  Path := Project2.Project_Path;
               else
                  --  Duplicate project base name.
                  File := GNATCOLL.VFS.No_File;
                  return;
               end if;
            end if;

            Next (Iterator);
         end loop;
      end if;

      if Path /= GNATCOLL.VFS.No_File then
         --  Found single project with given base name.
         File := Path;
         return;
      end if;

      --  We have to search in one or more projects

      if not Predefined_Only then
         if Project.Data /= null then
            Iterator := Project.Start (Recursive => False);
         else
            Iterator := Self.Root_Project.Start (Recursive => True);
         end if;

         while Path = GNATCOLL.VFS.No_File or else Duplicate_Obj loop
            --  Checking whenever we have an ambiguous object file.
            Project2 := Current (Iterator);
            exit when Project2 = No_Project;

            if Duplicate_Obj
              and then Locate_Regular_File
                (Name, Project2.Object_Path
                   (Recursive => False, Including_Libraries => True)) /=
                  GNATCOLL.VFS.No_File
            then
               File := GNATCOLL.VFS.No_File;
               return;
            end if;

            if not Duplicate_Obj and then Use_Source_Path then
               --  No need to check for object duplicates in source dirs.
               Path := Locate_Regular_File
                 (Name, Project2.Source_Dirs (Recursive => False));
            end if;

            if Use_Object_Path
              and then not Duplicate_Obj
              and then Path = GNATCOLL.VFS.No_File
            then
               --  We do not want to loose Path in the check fails.
               Path := Locate_Regular_File
                 (Name, Project2.Object_Path
                    (Recursive => False, Including_Libraries => True));

               if Path /= GNATCOLL.VFS.No_File
                 and then Is_Aggregate_Project (Self.Root_Project)
                 and then Project.Data = null
               then
                  --  Check is only relevant when root project is aggregate and
                  --  no project has been given as an argument.
                  Duplicate_Obj := True;
               end if;
            end if;

            Next (Iterator);
         end loop;
      end if;

      --  Only search in the predefined directories if the user did not
      --  specify an explicit project

      if Path = GNATCOLL.VFS.No_File and then Project.Data = null then
         if Use_Source_Path
           and then Self.Data.Env.Predefined_Source_Path /= null
         then
            Project2 := No_Project;
            Path := Locate_Regular_File
              (Name, Self.Data.Env.Predefined_Source_Path.all);
         end if;

         if Use_Object_Path
           and then Path = GNATCOLL.VFS.No_File
           and then Self.Data.Env.Predefined_Object_Path /= null
         then
            Project2 := No_Project;
            Path := Locate_Regular_File
              (Name, Self.Data.Env.Predefined_Object_Path.all);
         end if;

         In_Predefined := Path /= GNATCOLL.VFS.No_File;
      end if;

      --  If still not found, search in the current directory

      if Path = GNATCOLL.VFS.No_File then
         Project2 := No_Project;
         In_Predefined := False;
         Path := Locate_Regular_File (Name, (1 => Get_Current_Dir));
      end if;

      --  If found, cache the result for future usage.
      --  We do not cache anything if the project was forced, however
      --  since this wouldn't work with extended projects were sources
      --  can be duplicated.
      --  Do not cache either files found in the current directory, since that
      --  could change.
      --
      --  ??? There is a potential issue if for instance we found the file in
      --  a source dir but the next call specifies Use_Source_Path=>False. But
      --  that's an unlikely scenario because the user knows where to expect a
      --  file in general.

      if Path /= GNATCOLL.VFS.No_File
        and then Project.Data = null
        and then (Project2 /= No_Project   --  found in a specific project
                  or else In_Predefined)   --  or in the runtime

        --  Make sure the predefined file does not hide a project source
        --  (since we bypassed the cached above when Predefined_Only is true)
        and then (not Predefined_Only
                  or else not Tree_For_Map.Sources.Contains (Base))
      then
         --  Language and Source are always unknown: if we had a source file,
         --  it would have been set in the cache while loading the project.
         --  However, for runtime files we do compute the language since these
         --  are likely to be source files

         Source_Info := Source_File_Data'
           (Project => No_Project,  --  file is not a source
            File    => Path,
            Lang    => No_Name,
            Source  => null,
            Next => null);

         if In_Predefined then
            Source_Info.Lang := Get_String (Language (Info (Self.Data, Path)));
         end if;

         Include_File (Tree_For_Map.Sources, Base, Source_Info);
      end if;

      File := Path;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out File_And_Project_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_And_Project_Array, File_And_Project_Array_Access);
   begin
      Unchecked_Free (Self);
   end Free;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Project               : Project_Type;
      Recursive             : Boolean := False;
      Include_Project_Files : Boolean := False)
      return File_And_Project_Array_Access
   is
      Count   : Natural := 0;
      Index   : Natural;
      P       : Project_Type;
      Result  : File_And_Project_Array_Access;
      Iter    : Project_Iterator := Start (Project, Recursive => Recursive);
   begin
      --  Count files

      loop
         P := Current (Iter);
         exit when P = No_Project;

         if P.Data.Files /= null then
            Count := Count + P.Data.Files'Length;
         end if;

         if Include_Project_Files then
            Count := Count + 1;
         end if;

         Next (Iter);
      end loop;

      Result := new File_And_Project_Array (1 .. Count);
      Index := Result'First;

      Iter := Start (Project, Recursive => Recursive);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         if Include_Project_Files then
            Result (Index) :=
               (File    => P.Project_Path,
                Project => P);
            Index := Index + 1;
         end if;

         if P.Data.Files /= null then
            for S in P.Data.Files'Range loop
               Result (Index) :=
                 (File    => P.Data.Files (S),
                  Project => P);
               Index := Index + 1;
            end loop;
         end if;

         Next (Iter);
      end loop;

      return Result;
   end Source_Files;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Project                  : Project_Type;
      Recursive                : Boolean := False;
      Include_Externally_Built : Boolean := True)
      return GNATCOLL.VFS.File_Array_Access
   is
      Count   : Natural := 0;
      Index   : Natural := 1;
      P       : Project_Type;
      Sources : File_Array_Access;

   begin
      if not Recursive then
         if Project.Data = null
           or else Project.Data.Files = null
           or else (not Include_Externally_Built
                    and then Externally_Built (Project))
         then
            return new File_Array (1 .. 0);
         else
            return new File_Array'(Project.Data.Files.all);
         end if;
      end if;

      declare
         Iter : Project_Iterator := Start (Project, Recursive);
      begin
         --  Count files

         loop
            P := Current (Iter);
            exit when P = No_Project;

            --  Files may be null in case of a parse error

            if P.Data.Files /= null
              and then (Include_Externally_Built
                        or else not Externally_Built (P))
            then
               Count := Count + P.Data.Files'Length;
            end if;

            Next (Iter);
         end loop;

         Sources := new File_Array (1 .. Count);
         Iter    := Start (Project, Recursive);

         --  Now add files to the Sources array

         loop
            P := Current (Iter);
            exit when P = No_Project;

            if P.Data.Files /= null
              and then (Include_Externally_Built
                        or else not Externally_Built (P))
            then
               for S in P.Data.Files'Range loop
                  Sources (Index) := P.Data.Files (S);
                  Index := Index + 1;
               end loop;
            end if;

            Next (Iter);
         end loop;

         Sort (Sources.all);

         return Sources;
      end;
   end Source_Files;

   ---------------
   -- Unit_Part --
   ---------------

   function Unit_Part (Info : File_Info'Class) return Unit_Parts is
   begin
      return Info.Part;
   end Unit_Part;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Info : File_Info'Class) return String is
   begin
      if Info.Name = No_Name then
         return "";
      else
         return Get_String (Info.Name);
      end if;
   end Unit_Name;

   --------------
   -- Language --
   --------------

   function Language (Info : File_Info'Class) return String is
   begin
      if Info.Lang = No_Name then
         --  This is likely a file from the predefined search path, for which
         --  no project information is available. Most likely from the Ada
         --  runtime.
         --  ??? Should we return "ada"
         return "";
      else
         return Get_String (Info.Lang);
      end if;
   end Language;

   -------------
   -- Project --
   -------------

   function Project
     (Info              : File_Info'Class;
      Root_If_Not_Found : Boolean := False) return Project_Type
   is
   begin
      if Root_If_Not_Found and then Info.Project = No_Project then
         return Info.Root_Project;
      else
         return Info.Project;
      end if;
   end Project;

   ----------
   -- Info --
   ----------

   function Info
     (Tree : Project_Tree_Data_Access;
      File : GNATCOLL.VFS.Virtual_File) return File_Info
   is
      Part : Unit_Parts;
      Id   : Source_Id;
      Full : String := String
        (File.Full_Name
           (Normalize     => True,
            Resolve_Links => not Tree.Env.Trusted_Mode).all);
      Path : Path_Name_Type;
      Lang : Name_Id;

   begin
      if File = GNATCOLL.VFS.No_File then
         return
           (File         => GNATCOLL.VFS.No_File,
            Project      => No_Project,
            Root_Project => Tree.Root,
            Part         => Unit_Separate,
            Name         => GPR.No_Name,
            Lang         => GPR.No_Name);
      end if;

      --  Lookup in the project's Source_Paths_HT, rather than in
      --  Registry.Data.Sources, since the latter does not support duplicate
      --  base names. In GPR.Nmsc, names have been converted to lower case on
      --  case-insensitive file systems, so we need to do the same here.
      --  (Insertion is done in Check_File, where the Path passed in parameter
      --  comes from a call to Normalize_Pathname with the following args:
      --      Resolve_Links  => Opt.Follow_Links_For_Files
      --      Case_Sensitive => True
      --  So we use the normalized name in the above call to Full_Name for
      --  full compatibility between GPS and the project manager

      Osint.Canonical_Case_File_Name (Full);
      Path := Path_Name_Type (Name_Id'(Get_String (Full)));
      Id := Source_Paths_Htable.Get (Tree.View.Source_Paths_HT, Path);

      if Id /= No_Source then
         Part := Kind_To_Part (Id);
         if Id.Unit /= null then
            return File_Info'
              (Project      => Project_Type
                 (Project_From_Name (Tree, Id.Project.Name)),
               Root_Project => Tree.Root,
               File         => File,
               Part         => Part,
               Name         => Id.Unit.Name,
               Lang         => Id.Language.Name);
         else
            return File_Info'
              (Project      => Project_Type
                 (Project_From_Name (Tree, Id.Project.Name)),
               Root_Project => Tree.Root,
               File         => File,
               Part         => Part,
               Name         => No_Name,
               Lang         => Id.Language.Name);
         end if;
      end if;

      --  Either the file was not cached, or there is no Source info. In both
      --  cases, that means the file is not a source file (although it might be
      --  a predefined source file), so we just use the default naming scheme.

      declare
         Ext    : constant Filesystem_String :=
           File.File_Extension (Normalize => True);
         Cursor : Extensions_Languages.Cursor;
         NS     : Naming_Scheme_Access;
      begin
         if Ext = ".ads" then
            --  Do not compute the unit names, which requires parsing the file
            --  or the ALI file, since the GNAT runtime uses krunched names
            return File_Info'
              (Project      => No_Project,
               Root_Project => Tree.Root,
               File         => File,
               Part         => Unit_Spec,
               Name         => GPR.No_Name,
               Lang         => Name_Ada);

         elsif Ext = ".adb" then
            return File_Info'
              (Project      => No_Project,
               Root_Project => Tree.Root,
               File         => File,
               Part         => Unit_Body,
               Name         => GPR.No_Name,
               Lang         => Name_Ada);
         end if;

         --  Try and guess the language from the registered extensions

         if Ext = "" then
            --  This is a file without extension like Makefile or
            --  ChangeLog for example. Use the filename to get the proper
            --  language for this file.

            Cursor := Tree.Env.Extensions.Find (Base_Name (Full));

         else
            Cursor := Tree.Env.Extensions.Find (+Ext);
         end if;

         if Has_Element (Cursor) then
            Lang := Extensions_Languages.Element (Cursor);
         else
            Lang := GPR.No_Name;
            NS := Tree.Env.Naming_Schemes;
            while NS /= null loop
               if +Ext = NS.Default_Spec_Suffix.all
                 or else +Ext = NS.Default_Body_Suffix.all
               then
                  Lang := Get_String (NS.Language.all);
                  exit;
               end if;

               NS := NS.Next;
            end loop;
         end if;
      end;

      return
        (File         => GNATCOLL.VFS.No_File,
         Project      => No_Project,
         Root_Project => Tree.Root,
         Part         => Unit_Separate,
         Name         => GPR.No_Name,
         Lang         => Lang);
   end Info;

   ----------
   -- Info --
   ----------

   function Info
     (Self : Project_Tree'Class; File : GNATCOLL.VFS.Virtual_File)
      return File_Info is
   begin
      if Self.Data = null then
         raise Program_Error with "no project tree was parsed";
      end if;

      if Is_Aggregate_Project (Self.Data.Root) then
         raise Program_Error with "root project is aggregate, cannot use Info";
      end if;

      return Info (Self.Data, File);
   end Info;

   --------------
   -- Info_Set --
   --------------

   function Info_Set
     (Self : Project_Tree'Class; File : GNATCOLL.VFS.Virtual_File)
      return File_Info_Set
   is
      M_Cur  : Names_Files.Cursor;
      B_Name : constant Filesystem_String := File.Base_Name;

      Source : Source_File_Data;
      S_Info : File_Info;

      Tree_For_Map : Project_Tree_Data_Access;

      Result : File_Info_Set :=
        (File_Info_Sets.Empty_Set with null record);

      function Unit_Kind_To_Part (Src_Kind : GPR.Source_Kind)
                                  return Unit_Parts;
      --  Translate GPR.Source_Kind into Unit_Parts.

      function Unit_Kind_To_Part (Src_Kind : GPR.Source_Kind) return Unit_Parts
      is
      begin
         case Src_Kind is
            when Spec =>
               return Unit_Spec;
            when Impl =>
               return Unit_Body;
            when Sep =>
               return Unit_Separate;
         end case;
      end Unit_Kind_To_Part;

   begin
      if Self.Data = null then
         raise Program_Error with "no project tree was parsed";
      end if;

      Tree_For_Map := Self.Data.Root.Data.Tree_For_Map;
      M_Cur := Tree_For_Map.Sources.Find (B_Name);

      if M_Cur = Names_Files.No_Element then
         Result.Include (Info (Self.Data, File));
         return Result;
      end if;

      Source := Names_Files.Element (M_Cur);

      loop
         if Source.File = File then
            S_Info.Project := Source.Project;
            S_Info.Root_Project := Self.Root_Project;
            S_Info.File := File;

            if Source.Source /= null then
               --  One of the initially cached source files.
               S_Info.Part := Unit_Kind_To_Part (Source.Source.Kind);
               if Source.Source.Unit = No_Unit_Index then
                  --  Not applicable to C and other non unit-based languages.
                  S_Info.Name := No_Name;
               else
                  S_Info.Name := Source.Source.Unit.Name;
               end if;
               S_Info.Lang := Source.Source.Language.Name;

            else
               --  Cached after a call to create, thus no Source_Id. The only
               --  thing known is the language.

               declare
                  Tmp_Info : constant File_Info := Info (Self.Data, File);
               begin
                  S_Info.Part := Tmp_Info.Part;
                  S_Info.Lang := Tmp_Info.Lang;
                  S_Info.Name := Tmp_Info.Name;
               end;
            end if;

            Result.Include (S_Info);
         end if;

         exit when Source.Next = null;
         Source := Source.Next.all;
      end loop;

      if Result.Is_Empty then
         --  The file does not belong to the project. However, we can still
         --  make some guesses regarding its language and various pieces of
         --  information.

         Result.Include (Info (Self.Data, File));
      end if;

      return Result;
   end Info_Set;

   ----------
   -- File --
   ----------

   function File (Info : File_Info'Class) return GNATCOLL.VFS.Virtual_File is
   begin
      return Info.File;
   end File;

   --------------------
   -- Substitute_Dot --
   --------------------

   function Substitute_Dot
     (Unit_Name       : String;
      Dot_Replacement : String) return String
   is
      Dot_Count : Natural := 0;
   begin
      for U in Unit_Name'Range loop
         if Unit_Name (U) = '.' then
            Dot_Count := Dot_Count + 1;
         end if;
      end loop;

      declare
         Uname : String
           (1 .. Unit_Name'Length + Dot_Count * (Dot_Replacement'Length - 1));
         Index : Natural := Uname'First;
      begin
         for U in Unit_Name'Range loop
            if Unit_Name (U) = '.' then
               Uname (Index .. Index + Dot_Replacement'Length - 1) :=
                 Dot_Replacement;
               Index := Index + Dot_Replacement'Length;
            else
               Uname (Index) := Unit_Name (U);
               Index := Index + 1;
            end if;
         end loop;
         return Uname;
      end;
   end Substitute_Dot;

   --------------------
   -- File_From_Unit --
   --------------------

   function File_From_Unit
     (Project                  : Project_Type;
      Unit_Name                : String;
      Part                     : Unit_Parts;
      Language                 : String;
      File_Must_Exist          : Boolean := True) return Filesystem_String
   is
      function Has_Predefined_Prefix (S : String) return Boolean;
      --  Return True is S has a name that starts like a predefined unit
      --  (e.g. a.b, which should be replaced by a~b)

      ---------------------------
      -- Has_Predefined_Prefix --
      ---------------------------

      function Has_Predefined_Prefix (S : String) return Boolean is
         C : constant Character := S (S'First);
      begin
         return S (S'First + 1) = '-'
           and then (C = 'a' or else C = 'g' or else C = 'i' or else C = 's');
      end Has_Predefined_Prefix;

      Unit    : Name_Id;
      UIndex  : Unit_Index;
      Lang    : Language_Ptr;
   begin

      if Is_Ada_Predefined_Unit (Unit_Name) then
         declare
            Buffer : String := To_Lower (Substitute_Dot (Unit_Name, "-"));
            Len    : Natural := Buffer'Length;
         begin
            pragma Assert (Buffer'First = 1);
            GNATCOLL.Projects.Krunch.Krunch
              (Buffer, Len, Maxlen => Buffer'Length,
               No_Predef => False);

            case Part is
            when Unit_Body | Unit_Separate =>
               return +Buffer (1 .. Len) & ".adb";
            when Unit_Spec =>
               return +Buffer (1 .. Len) & ".ads";
            end case;
         end;
      end if;

      --  Standard GNAT naming scheme
      --  ??? This isn't language independent, what if other languages have
      --  similar requirements. Should use configuration files as gprbuild does

      if Project = No_Project then
         if Language = "ada" then
            case Part is
            when Unit_Body =>
               return +Substitute_Dot (Unit_Name, "-") & ".adb";
            when Unit_Spec =>
               return +Substitute_Dot (Unit_Name, "-") & ".ads";
            when others =>
               Assert (Me, False, "Unexpected Unit_Part");
               return "";
            end case;
         else
            return "";
         end if;

      --  The project naming scheme
      else
         Unit := Get_Lower_Name_Id (Unit_Name);

         --  Take advantage of computation done by the project manager when we
         --  looked for source files

         UIndex := Units_Htable.Get (Project.Tree_View.Units_HT, Unit);
         if UIndex /= No_Unit_Index then
            case Part is
               when Unit_Body | Unit_Separate =>
                  if UIndex.File_Names (Impl) /= null then
                     return +Get_String (UIndex.File_Names (Impl).File);
                  end if;

               when Unit_Spec =>
                  if UIndex.File_Names (Spec) /= null then
                     return +Get_String (UIndex.File_Names (Spec).File);
                  end if;
            end case;
         end if;

         --  The unit does not exist yet. Perhaps we are creating a new file
         --  and trying to guess the correct file name

         if File_Must_Exist then
            return "";
         end if;

         --  We can only perform guesses if the language is a valid for the
         --  project.

         Lang := Get_Language_From_Name (Get_View (Project), Language);

         if Lang = null then
            return "";
         end if;

         declare
            Dot_Replacement : constant String := Get_String
              (Name_Id (Lang.Config.Naming_Data.Dot_Replacement));
            Uname           : String := Substitute_Dot
              (Unit_Name, Dot_Replacement);

         begin
            case Lang.Config.Naming_Data.Casing is
               when All_Lower_Case => GNAT.Case_Util.To_Lower (Uname);
               when All_Upper_Case => GNAT.Case_Util.To_Upper (Uname);
               when others => null;
            end case;

            --  Handle properly special naming such as a.b -> a~b

            if Case_Insensitive_Equal (Language, "ada")
              and then Uname'Length > 2
              and then Has_Predefined_Prefix (Uname)
            then
               Uname (Uname'First + 1) := '~';
            end if;

            case Part is
               when Unit_Body =>
                  return +(Uname
                           & Get_Name_String
                             (Name_Id (Lang.Config.Naming_Data.Body_Suffix)));

               when Unit_Spec =>
                  return +(Uname
                           & Get_Name_String
                             (Name_Id (Lang.Config.Naming_Data.Spec_Suffix)));

               when Unit_Separate =>
                  return +(Uname
                           & Get_Name_String
                             (Name_Id
                                (Lang.Config.Naming_Data.Separate_Suffix)));
            end case;
         end;
      end if;
   end File_From_Unit;

   ----------------
   -- Other_File --
   ----------------

   function Other_File
     (Self : Project_Tree;
      File : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File
   is
      --  Should we ask the user for the project ?
      --  in practice, it is likely that the other file is in the same
      --  project, so whichever project tree we choose we would likely end up
      --  with the same other file.

      Info : constant File_Info :=
        File_Info (Self.Info_Set (File).First_Element);
      Unit : constant String := Unit_Name (Info);
      Part : Unit_Parts;

      function Test_Suffixes
        (Old_Suffix, New_Suffix : String) return Virtual_File;
      --  Substitute prefixes and check whether the file exists

      function Non_Unit_Based
        (Old_Part, New_Part : Attribute_Pkg_String) return Virtual_File;
      --  Handling of non-unit based languages

      -------------------
      -- Test_Suffixes --
      -------------------

      function Test_Suffixes
        (Old_Suffix, New_Suffix : String) return Virtual_File
      is
         Other_F : constant Virtual_File := Self.Create
           (File.Base_Name (+Old_Suffix) & (+New_Suffix),
            Use_Object_Path => False);
      begin
         if Other_F = GNATCOLL.VFS.No_File then
            return File;
         else
            return Other_F;
         end if;
      end Test_Suffixes;

      --------------------
      -- Non_Unit_Based --
      --------------------

      function Non_Unit_Based
        (Old_Part, New_Part : Attribute_Pkg_String) return Virtual_File
      is
         Suffix : constant String :=
           Info.Project.Attribute_Value (Old_Part, Index => Info.Language);
         New_Suffix : constant String :=
           Info.Project.Attribute_Value (New_Part, Index => Info.Language);
      begin
         return Test_Suffixes (Suffix, New_Suffix);
      end Non_Unit_Based;

   begin
      case Info.Part is
         when Unit_Spec                 => Part := Unit_Body;
         when Unit_Body | Unit_Separate => Part := Unit_Spec;
      end case;

      --  Do we have a unit-based language ?

      if Unit /= "" then

         --  Is there such a file in the project ?
         declare
            Base : constant Filesystem_String := File_From_Unit
              (Project (Info), Unit, Part,
               Language => Info.Language);
         begin
            if Base'Length > 0 then
               return Self.Create (Base, Use_Object_Path => False);
            end if;
         end;

         --  Special case for separate units, since the spec is a parent
         --  package

         if Info.Part = Unit_Separate then
            for J in reverse Unit'Range loop
               if Unit (J) = '.' then
                  declare
                     Base : constant Filesystem_String := File_From_Unit
                       (Project (Info), Unit (Unit'First .. J - 1), Unit_Spec,
                        Language => Info.Language);
                  begin
                     if Base'Length > 0 then
                        return Self.Create (Base, Use_Object_Path => False);
                     end if;
                  end;
               end if;
            end loop;
         end if;

         --  Second special case for separate units. When no parent spec has
         --  been found, there still exists a scenario when separate is from
         --  a body unit that does not have a spec. We need an extra loop
         --  to not wrongly pick up the case when there is a chain of separates
         --  one declared in another.

         if Info.Part = Unit_Separate then
            for J in reverse Unit'Range loop
               if Unit (J) = '.' then
                  declare
                     Base : constant Filesystem_String := File_From_Unit
                       (Project (Info), Unit (Unit'First .. J - 1), Unit_Body,
                        Language => Info.Language);
                  begin
                     if Base'Length > 0 then
                        return Self.Create (Base, Use_Object_Path => False);
                     end if;
                  end;
               end if;
            end loop;
         end if;

         --  Else try to guess from naming scheme

         declare
            Base : constant Filesystem_String := File_From_Unit
              (Project (Info), Unit, Part,
               Language => Info.Language, File_Must_Exist => False);
         begin
            if Base'Length > 0 then
               return GNATCOLL.VFS.Create_From_Dir
                 (Dir       => Create (Dir_Name (File)),
                  Base_Name => Base);
            end if;
         end;

         --  Else try the default GNAT naming scheme for runtime files

         if Case_Insensitive_Equal (Info.Language, "ada") then
            declare
               Base : constant Filesystem_String := File_From_Unit
                 (Project (Info), Unit, Part,
                  Language => Info.Language);
            begin
               if Base'Length > 0 then
                  return Self.Create (Base, Use_Object_Path => False);
               end if;
            end;
         end if;
      end if;

      --  Else simply try switching the extensions (useful for krunched names)
      --  for unit-based languages.
      --  For non-unit based languages, we only guess the "other file" if it
      --  actually exists in the project. We never try to create one, since
      --  there is no insurance the user needs one or its name will be
      --  consistent.

      if Info.Project = No_Project then
         case Info.Part is
            when Unit_Spec =>
               return Test_Suffixes (".ads", ".adb");
            when Unit_Body | Unit_Separate =>
               return Test_Suffixes (".adb", ".ads");
         end case;

      else
         case Info.Part is
            when Unit_Spec =>
               return Non_Unit_Based
                 (Spec_Suffix_Attribute, Impl_Suffix_Attribute);
            when Unit_Body | Unit_Separate =>
               return Non_Unit_Based
                 (Impl_Suffix_Attribute, Spec_Suffix_Attribute);
         end case;
      end if;
   end Other_File;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : String;
      Index        : String := "";
      Use_Extended : Boolean := False) return Variable_Value
   is
      Sep            : constant Natural :=
        Ada.Strings.Fixed.Index (Attribute, "#");
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Project_View   : constant Project_Id := Get_View (Project);
      Pkg            : Package_Id := No_Package;
      Value          : Variable_Value := Nil_Variable_Value;
      Var            : Variable_Id;
      Arr            : Array_Id;
      Elem           : Array_Element_Id;
      N              : Name_Id;
      Shared         : Shared_Project_Tree_Data_Access;
   begin
      if Project_View = GPR.No_Project then
         return Nil_Variable_Value;
      end if;

      Shared := Project.Tree_View.Shared;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            Shared      => Shared);

         if Pkg = No_Package then
            if Use_Extended
              and then Extended_Project (Project) /= No_Project
            then
               return Attribute_Value
                 (Extended_Project (Project), Attribute, Index, Use_Extended);
            else
               return Nil_Variable_Value;
            end if;
         end if;

         Var := Shared.Packages.Table (Pkg).Decl.Attributes;
         Arr := Shared.Packages.Table (Pkg).Decl.Arrays;

      else
         Var := Project_View.Decl.Attributes;
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);

      if Index /= "" then
         Elem := Value_Of (N, In_Arrays => Arr, Shared => Shared);
         if Elem /= No_Array_Element then
            Value := Value_Of
              (Index                  => Get_String (Index),
               In_Array               => Elem,
               Shared                 => Shared,
               Force_Lower_Case_Index => Project.Has_Language (Index));
         end if;
      else
         Value := Value_Of (N, In_Variables => Var, Shared => Shared);
      end if;

      if Value.Location = No_Location
        and then Use_Extended
        and then Extended_Project (Project) /= No_Project
      then
         return Attribute_Value
           (Extended_Project (Project), Attribute, Index, Use_Extended);
      else
         return Value;
      end if;
   end Attribute_Value;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_String;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String
   is
      View  : constant Project_Id := Get_View (Project);
      Value : Variable_Value;
      Lang  : Language_Ptr;
      Unit  : Unit_Index;
   begin
      if Project = No_Project or else View = GPR.No_Project then
         return Default;
      end if;

      --  Special case for the naming scheme, since we need to get access to
      --  the default registered values for foreign languages

      if Attribute = Spec_Suffix_Attribute
        or else Attribute = Specification_Suffix_Attribute
      then
         Lang := Get_Language_From_Name (View, Index);
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Spec_Suffix);
         else
            declare
               Default : constant String :=
                 Default_Spec_Suffix (Project.Data.Tree.Env.all, Index);
            begin
               if Default = Dummy_Suffix then
                  return "";
               else
                  return Default;
               end if;
            end;
         end if;

      elsif Attribute = Impl_Suffix_Attribute
        or else Attribute = Implementation_Suffix_Attribute
      then
         Lang := Get_Language_From_Name (View, Index);
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Body_Suffix);
         else
            declare
               Default : constant String :=
                 Default_Body_Suffix (Project.Data.Tree.Env.all, Index);
            begin
               if Default = Dummy_Suffix then
                  return "";
               else
                  return Default;
               end if;
            end;
         end if;

      elsif Attribute = Separate_Suffix_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Separate_Suffix);
         else
            return "";
         end if;

      elsif Attribute = Casing_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return GPR.Image (Lang.Config.Naming_Data.Casing);
         else
            return "";
         end if;

      elsif Attribute = Dot_Replacement_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Dot_Replacement);
         else
            return "";
         end if;

      elsif Attribute = Old_Implementation_Attribute
        or else Attribute = Body_Attribute
      then
         --  Index is a unit name
         Unit := Units_Htable.Get
           (Project.Tree_View.Units_HT,
            Get_String (Index));
         if Unit /= No_Unit_Index
           and then Unit.File_Names (Impl) /= null
         then
            if Unit.File_Names (Impl).Index /= 0 then
               return Get_String (Unit.File_Names (Impl).Display_File)
                 & " at" & Unit.File_Names (Impl).Index'Img;
            else
               return Get_String (Unit.File_Names (Impl).Display_File);
            end if;
         else
            --  We might have a separate or some other value. Fallback to
            --  looking in the attribute itself (but this won't handle the
            --  Index part -- perhaps separates are not usable in a multi-unit
            --  source file, which would seem logical anyway)
            null;
         end if;

      elsif Attribute = Old_Specification_Attribute
        or else Attribute = Spec_Attribute
      then
         --  Index is a unit name
         Unit := Units_Htable.Get
           (Project.Tree_View.Units_HT, Get_String (Index));
         if Unit /= No_Unit_Index
           and then Unit.File_Names (Spec) /= null
         then
            if Unit.File_Names (Spec).Index /= 0 then
               return Get_String (Unit.File_Names (Spec).Display_File)
                 & " at" & Unit.File_Names (Spec).Index'Img;
            else
               return Get_String (Unit.File_Names (Spec).Display_File);
            end if;
         else
            return "";
         end if;
      end if;

      Value := Attribute_Value
        (Project, String (Attribute), Index, Use_Extended);

      case Value.Kind is
         when Undefined => return Default;
         when Single    => return Value_Of (Value, Default);
         when List      =>
            Trace (Me, "Attribute " & String (Attribute)
                   & " is not a single string");
            return Default;
      end case;
   end Attribute_Value;

   -----------------------
   -- Attribute_Project --
   -----------------------

   function Attribute_Project
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Index     : String := "") return Project_Type
   is
      Value : constant Variable_Value :=
                Attribute_Value (Project, String (Attribute), Index);
      Tree : constant Project_Tree := (Data => Project.Data.Tree);
   begin
      if Value.Project = GPR.No_Project then
         return No_Project;
      else
         declare
            Name : constant String := Get_Name_String (Value.Project.Name);
         begin
            return Tree.Project_From_Name (Name);
         end;
      end if;
   end Attribute_Project;

   --------------------------
   -- Attribute_Registered --
   --------------------------

   function Attribute_Registered (Name : String; Pkg : String) return Boolean
   is
      Lower_Pkg : constant String := To_Lower (Pkg);
      Pkg_Id    : Package_Node_Id := Empty_Package;
   begin
      --  Need to make sure the predefined packages are already declared, or
      --  the new one will be discarded.

      GPR.Attr.Initialize;

      if Lower_Pkg = "" then
         Trace (Me, "Attribute_Registered called for empty package");
         return True;
      end if;

      Pkg_Id := Package_Node_Id_Of (Get_String (Lower_Pkg));
      if Pkg_Id = Empty_Package or else Pkg_Id = Unknown_Package then
         --  We don't even have such a package.
         return False;
      end if;

      return GPR.Attr.Attribute_Registered (Name, Pkg_Id);
   end Attribute_Registered;

   ----------------------------
   -- Variable_Value_To_List --
   ----------------------------

   function Variable_Value_To_List
     (Project : Project_Type;
      Value   : Variable_Value) return GNAT.Strings.String_List_Access
   is
      V      : String_List_Id;
      S      : String_List_Access;
      Shared : Shared_Project_Tree_Data_Access;
   begin
      case Value.Kind is
         when Undefined =>
            return null;

         when Single =>
            --  ??? Should we really convert to a list
            return new String_List'
              (1 .. 1 => new String'(Get_Name_String (Value.Value)));

         when List =>
            S := new String_List
              (1 .. Length (Project.Tree_View, Value.Values));
            V := Value.Values;

            Shared := Project.Tree_View.Shared;
            for J in S'Range loop
               Get_Name_String (Shared.String_Elements.Table (V).Value);
               S (J) := new String'(Name_Buffer (1 .. Name_Len));
               V := Shared.String_Elements.Table (V).Next;
            end loop;
            return S;
      end case;
   end Variable_Value_To_List;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_List;
      Index        : String := "";
      Use_Extended : Boolean := False) return GNAT.Strings.String_List_Access
   is
      Value : constant Variable_Value := Attribute_Value
        (Project, String (Attribute), Index, Use_Extended);
   begin
      return Variable_Value_To_List (Project, Value);
   end Attribute_Value;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Project   : Project_Type;
      Attribute : String;
      Index     : String := "") return Boolean
   is
      Shared         : Shared_Project_Tree_Data_Access;
      Sep            : constant Natural :=
                         Ada.Strings.Fixed.Index (Attribute, "#");
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Project_View   : constant Project_Id := Get_View (Project);
      Pkg            : Package_Id := No_Package;
      Var            : Variable_Id;
      Arr            : Array_Id;
      N, I           : Name_Id;
      Arr_Elem_Id    : Array_Element_Id;
   begin
      if Project_View = GPR.No_Project then
         return False;
      end if;

      Shared := Project.Tree_View.Shared;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            Shared      => Shared);

         if Pkg = No_Package then
            Trace (Me, "No such package " & Pkg_Name);
            return False;
         end if;

         Var := Shared.Packages.Table (Pkg).Decl.Attributes;
         Arr := Shared.Packages.Table (Pkg).Decl.Arrays;

      else
         Var := Project_View.Decl.Attributes;
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);

      if Index /= "" then
         --  ??? That seems incorrect, we are not testing for the specific
         --  index
         Arr_Elem_Id := Value_Of (N, In_Arrays => Arr, Shared => Shared);
         if Arr_Elem_Id = No_Array_Element then
            return False;
         end if;

         I := Get_String (Index);
         return Value_Of
           (I,
            In_Array               => Arr_Elem_Id,
            Shared                 => Shared,
            Force_Lower_Case_Index => Project.Has_Language (Index)) /=
             Nil_Variable_Value;
      else
         return not Value_Of (N, Var, Shared).Default;
      end if;
   end Has_Attribute;

   function Has_Attribute
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Index     : String := "") return Boolean is
   begin
      return Has_Attribute (Project, String (Attribute), Index);
   end Has_Attribute;

   function Has_Attribute
     (Project   : Project_Type;
      Attribute : Attribute_Pkg_List;
      Index     : String := "") return Boolean is
   begin
      return Has_Attribute (Project, String (Attribute), Index);
   end Has_Attribute;

   -----------------------
   -- Attribute_Indexes --
   -----------------------

   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : String;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List
   is
      Shared         : Shared_Project_Tree_Data_Access;
      Sep            : constant Natural :=
                         Ada.Strings.Fixed.Index (Attribute, "#");
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Project_View   : constant Project_Id := Get_View (Project);
      Packages       : GPR.Package_Table.Table_Ptr;
      Array_Elements : GPR.Array_Element_Table.Table_Ptr;
      Pkg            : Package_Id := No_Package;
      Arr            : Array_Id;
      Elem, Elem2    : Array_Element_Id;
      N              : Name_Id;
      Count          : Natural := 0;

   begin
      if Project_View = GPR.No_Project then
         return (1 .. 0 => null);
      end if;

      Shared         := Project.Tree_View.Shared;
      Packages       := Shared.Packages.Table;
      Array_Elements := Shared.Array_Elements.Table;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            Shared      => Shared);

         if Pkg = No_Package then
            if Use_Extended
              and then Extended_Project (Project) /= No_Project
            then
               return Attribute_Indexes
                 (Extended_Project (Project), Attribute, Use_Extended);
            else
               return (1 .. 0 => null);
            end if;
         end if;

         Arr := Packages (Pkg).Decl.Arrays;

      else
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);
      Elem := Value_Of (N, In_Arrays => Arr, Shared => Shared);
      if Elem = No_Array_Element
        and then Use_Extended
        and then Extended_Project (Project) /= No_Project
      then
         return Attribute_Indexes
           (Extended_Project (Project), Attribute, Use_Extended);
      end if;

      Elem2 := Elem;
      while Elem2 /= No_Array_Element loop
         Count := Count + 1;
         Elem2 := Array_Elements (Elem2).Next;
      end loop;

      declare
         Result : String_List (1 .. Count);
      begin
         Count := Result'First;

         while Elem /= No_Array_Element loop
            Result (Count) := new String'
              (Get_String (Array_Elements (Elem).Index));
            Count := Count + 1;
            Elem := Array_Elements (Elem).Next;
         end loop;

         return Result;
      end;
   end Attribute_Indexes;

   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_String;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List is
   begin
      return Attribute_Indexes (Project, String (Attribute), Use_Extended);
   end Attribute_Indexes;

   function Attribute_Indexes
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg_List;
      Use_Extended : Boolean := False) return GNAT.Strings.String_List is
   begin
      return Attribute_Indexes (Project, String (Attribute), Use_Extended);
   end Attribute_Indexes;

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (S : String) return String is
      Normalized : String := S;
   begin
      GNAT.Case_Util.To_Mixed (Normalized);
      return Normalized;
   end To_Mixed;

   ---------------
   -- Languages --
   ---------------

   procedure Languages
     (Project   : Project_Type;
      Recursive : Boolean := False;
      Langs     : in out Language_Sets.Set)
   is
      Iter : Inner_Project_Iterator;
      Val  : Variable_Value;
      P    : Project_Type;
   begin
      if Get_View (Project) = GPR.No_Project then
         return;
      end if;

      Iter := Start (Project, Recursive);

      declare
         Value : String_List_Id;
      begin
         loop
            P := Current (Iter);
            exit when P = No_Project;

            if P.Has_Attribute (Languages_Attribute) then
               Val := Attribute_Value (P, String (Languages_Attribute));
               case Val.Kind is
                  when Undefined => null;
                  when Single    =>
                     Langs.Include (To_Mixed (Get_Name_String (Val.Value)));
                  when List      =>
                     Value := Val.Values;
                     while Value /= Nil_String loop
                        Langs.Include
                          (To_Mixed (Get_String
                           (String_Elements (P.Data.Tree)(Value).Value)));
                        Value := String_Elements (P.Data.Tree)(Value).Next;
                     end loop;
               end case;

            else
               Langs.Include (To_Mixed ("ada"));
            end if;

            Next (Iter);
         end loop;
      end;
   end Languages;

   ---------------
   -- Languages --
   ---------------

   function Languages
     (Project : Project_Type; Recursive : Boolean := False) return String_List
   is
      Langs : Language_Sets.Set := Language_Sets.Empty_Set;
   begin
      if Project = No_Project or else Get_View (Project) = GPR.No_Project then
         return String_List'(1 .. 1 => new String'("Ada"));
      end if;

      --  Languages for the current project and its imported project
      Languages (Project, Recursive, Langs);

      if Project.Is_Aggregate_Project then
         declare
            Aggr_Array : Project_Array_Access :=
              Project.Aggregated_Projects (Unwind_Aggregated => True);
         begin
            --  If this is an aggregate project
            for P of Aggr_Array.all loop
               Languages (P, Recursive, Langs);
            end loop;
            Unchecked_Free (Aggr_Array);
         end;
      end if;

      if Integer (Langs.Length) = 0 then
         --  Empty set, return Ada as the default language
         return String_List'(1 .. 1 => new String'("Ada"));
      else
         --  Convert the Set to a list of String Access
         declare
            Lang_List : String_List (1 .. Integer (Langs.Length));
            Idx       : Integer := Lang_List'First;
         begin

            for L of Langs loop
               Lang_List (Idx) := new String'(L);
               Idx := Idx + 1;
            end loop;
            return Lang_List;
         end;
      end if;
   end Languages;

   ------------------
   -- Has_Language --
   ------------------

   function Has_Language
     (Project : Project_Type; Language : String) return Boolean
   is
      Normalized_Lang : constant Name_Id := Get_String (To_Lower (Language));
      P               : constant Project_Id := Get_View (Project);
      Lang            : Language_Ptr;
   begin
      if P /= GPR.No_Project then
         Lang := P.Languages;
         while Lang /= null loop
            if Lang.Name = Normalized_Lang then
               return True;
            end if;

            Lang := Lang.Next;
         end loop;

      end if;
      return False;
   end Has_Language;

   -------------------------------
   -- Get_Automatic_Config_File --
   -------------------------------

   function Get_Automatic_Config_File
     (Self : Project_Environment) return Boolean is
   begin
      return Self.Autoconf;
   end Get_Automatic_Config_File;

   ------------------
   -- Get_Closures --
   ------------------

   procedure Get_Closures
     (Project                  : Project_Type;
      Mains                    : GNATCOLL.VFS.File_Array_Access;
      All_Projects             : Boolean := True;
      Include_Externally_Built : Boolean := False;
      Status                   : out Status_Type;
      Result                   : out GNATCOLL.VFS.File_Array_Access)
   is
      Mains_Str_List : String_Vectors.Vector;
      Closure_Status : GPR.Util.Status_Type;
      Closures_List  : String_Vectors.Vector;
   begin
      Trace (Me, "Get_Closures");

      Unchecked_Free (Result);

      if Mains = null or else Mains'Length = 0 or else Project = No_Project
      then
         Status := Error;
         return;
      end if;

      for I in Mains'Range loop
         Mains_Str_List.Append (Mains (I).Display_Base_Name);
      end loop;

      GPR.Util.Get_Closures
        (Project.Get_View, Project.Tree_View,
         Mains                    => Mains_Str_List,
         All_Projects             => All_Projects,
         Include_Externally_Built => Include_Externally_Built,
         Status                   => Closure_Status,
         Result                   => Closures_List);

      case Closure_Status is
         when Success =>
            Status := Success;
         when Incomplete_Closure =>
            Status := Incomplete_Closure;
         when others =>
            Trace
              (Me,
               "cannot get closure, "
               & GPR.Util.Status_Type'Image (Closure_Status));
            Status := Error;
            return;
      end case;

      if Closure_Status in Success | Incomplete_Closure then
         for Closure of Closures_List loop
            Append (Result, Create (+Closure));
         end loop;
      end if;
   end Get_Closures;

   ---------------------
   -- Get_Config_File --
   ---------------------

   function Get_Config_File
     (Self : Project_Environment)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Config_File;
   end Get_Config_File;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
     (Project         : Project_Type;
      Default_To_Host : Boolean := True) return String is

      Prj : Project_Type := Project;

      Target_From_Attribute : constant String := Project.Attribute_Value
        (Attribute    => Target_Attribute,
         Use_Extended => True);

      function Extract_From_Attribute
        (Attribute : Attribute_Pkg_String;
         Suffix    : String) return String;
      --  Attempt to extract target from the value of the given attribute,
      --  assuming the value is of the form <target><suffix>.

      ----------------------------
      -- Extract_From_Attribute --
      ----------------------------

      function Extract_From_Attribute
        (Attribute : Attribute_Pkg_String;
         Suffix    : String) return String
      is
         Val : constant String := Project.Attribute_Value
             (Attribute    => Attribute,
              Use_Extended => True);
         SL  : constant Natural := Suffix'Length;
      begin
         if Val'Length > Suffix'Length
           and then To_Lower (Val (Val'Last - SL + 1 .. Val'Last)) = Suffix
         then
            return Val (Val'First .. Val'Last - SL);
         end if;

         return "";
      end Extract_From_Attribute;

   begin
      --  What this explicitly set in the environment ?

      if Project.Data.Tree.Env.Forced_Target /= null then
         return Project.Data.Tree.Env.Forced_Target.all;
      end if;

      --  First check whether the "Target" attribute is explicitly given

      if Target_From_Attribute /= "" then
         --  The attribute target is defined and non-empty: look no further!
         --  But we need to clarify where does this attribute come from.
         --  It may be either declared in the project itself or in one of
         --  projects extending it, or it may be inherited from cgpr.
         --  In the last case we do not want to return it.
         while Prj /= No_Project loop
            declare
               Target_Value : constant Variable_Value :=
                 Value_Of
                   (Get_String ("target"),
                    Prj.Data.View.Decl.Attributes,
                    Prj.Data.Tree.View.Shared);
            begin
               if Target_Value.Project = Prj.Data.View then
                  return Target_From_Attribute;
               end if;
            end;
            Prj := Extended_Project (Prj);
         end loop;
      end if;

      --  Next: look for the legacy way of defining the target via
      --  the "gnat" in the package "ide". We expect something of the form
      --      "arm-eabi-gnat";
      --  and we assume the target is the first part.

      declare
         G : constant String := Extract_From_Attribute
           (GNAT_Attribute, "-gnat");
      begin
         if G /= "" then
            return G;
         end if;
      end;

      --  Also look, similarly, at the gnatls attribute, expecting something
      --  of the form "arm-eabi-gnatls"

      declare
         G : constant String := Extract_From_Attribute
           (Gnatlist_Attribute, "-gnatls");
      begin
         if G /= "" then
            return G;
         end if;
      end;

      --  Nothing? The target is not defined.

      if Default_To_Host then
         return Target_From_Attribute;
      else
         return "";
      end if;
   end Get_Target;

   -----------------
   -- Get_Runtime --
   -----------------

   function Get_Runtime (Project : Project_Type) return String is
      List : GNAT.Strings.String_List_Access;
      S    : String_Access;
   begin
      --  What this explicitly set in the environment ?

      if Project.Data.Tree.Env.Forced_Runtime /= null then
         return Project.Data.Tree.Env.Forced_Runtime.all;
      end if;

      --  First check whether the "Runtime" attribute is explicitly given

      declare
         Runtime : constant String := Project.Attribute_Value
           (Attribute    => Runtime_Attribute,
            Index        => "ada",
            Use_Extended => True);
      begin
         if Runtime /= "" then
            --  Got it!
            return Runtime;
         end if;
      end;

      --  Look for the legacy way of specifying the runtime as a --RTS
      --  argument in the builder switches.

      List := Project.Attribute_Value
        (Attribute    => Builder_Default_Switches_Attribute,
         Index        => "ada",
         Use_Extended => True);

      if List /= null then
         for L in List'Range loop
            S := List (L);
            if S /= null
              and then S'Length > 5
              and then To_Lower (S (S'First .. S'First + 5)) = "--rts="
            then
               return S (S'First + 6 .. S'Last);
            end if;
         end loop;
      end if;

      --  No runtime defined

      return "";
   end Get_Runtime;

   -------------------------
   -- Target_Same_As_Host --
   -------------------------

   function Target_Same_As_Host (Project : Project_Type) return Boolean is
      Tgt : constant String := Normalize_Target_Name (Project.Get_Target);
   begin
      if Tgt = "" then
         return True;
      end if;

      for T of Host_Targets_List loop
         if T = Tgt then
            return True;
         end if;
      end loop;

      return False;
   end Target_Same_As_Host;

   ------------------
   -- Is_Main_File --
   ------------------

   function Is_Main_File
     (Project        : Project_Type;
      File           : GNATCOLL.VFS.Filesystem_String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      Value :           String_List_Access :=
                Project.Attribute_Value (Attribute    => Main_Attribute,
                                         Use_Extended => True);
      B_File : constant GNATCOLL.VFS.Filesystem_String := Base_Name (File);
      Files  :          VFS.File_Array_Access;
      Source : Boolean := False;
   begin
      Trace (Me, (+File) & " vs " & (+B_File));

      if GNATCOLL.VFS_Utils.Is_Absolute_Path (File) then
         --  Check that given file is a source of Project first.
         Files := Project.Source_Files (Recursive => False);
         for F of Files.all loop
            if F.Full_Name = File then
               Source := True;
               exit;
            end if;
         end loop;
         Unchecked_Free (Files);
         if not Source then
            Free (Value);
            return False;
         end if;
      end if;

      for V in Value'Range loop
         if Equal
           (Value (V).all, +B_File, Case_Sensitive => Case_Sensitive)
         then
            Free (Value);
            return True;
         end if;
      end loop;

      Free (Value);
      return False;
   end Is_Main_File;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory
    (Project  : Project_Type;
     Callback : Get_Directory_Path_Callback) return Virtual_File
   is
   begin
      if Project = No_Project
        or else Get_View (Project) = GPR.No_Project
      then
         return GNATCOLL.VFS.No_File;

      else
         declare
            Dir : constant Filesystem_String := +Get_String
                    (Name_Id (Callback (Get_View (Project)).Display_Name));
         begin
            if Dir'Length > 0 then
               return Create (Name_As_Directory (Dir));
            else
               --  ??? Can't we simply access Object_Dir in the view ?

               declare
                  Path : constant File_Array := Project.Object_Path;
               begin
                  if Path'Length /= 0 then
                     return Path (Path'First);
                  else
                     return GNATCOLL.VFS.No_File;
                  end if;
               end;
            end if;
         end;
      end if;
   end Get_Directory;

   ---------------------------
   -- Executables_Directory --
   ---------------------------

   function Executables_Directory
     (Project : Project_Type) return Virtual_File
   is

      function Get_Exec_Directory_Callback
        (Project : GPR.Project_Id) return Path_Information;

      ----------------------------------
      -- Get_Exec_Directory_Callback  --
      ----------------------------------

      function Get_Exec_Directory_Callback
        (Project : GPR.Project_Id) return Path_Information is
      begin
         return Project.Exec_Directory;
      end Get_Exec_Directory_Callback;

   begin
      return Get_Directory (Project,
                            Get_Exec_Directory_Callback'Unrestricted_Access);
   end Executables_Directory;

   -----------------------
   -- Library_Directory --
   -----------------------

   function Library_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is

      function Get_Library_Dir_Callback
        (Project : GPR.Project_Id) return Path_Information;

      ------------------------------
      -- Get_Library_Dir_Callback --
      ------------------------------

      function Get_Library_Dir_Callback
        (Project : GPR.Project_Id) return Path_Information is
      begin
         return Project.Library_Dir;
      end Get_Library_Dir_Callback;

   begin
      return Get_Directory (Project,
                            Get_Library_Dir_Callback'Unrestricted_Access);
   end Library_Directory;

   ---------------------------
   -- Library_Ali_Directory --
   ---------------------------

   function Library_Ali_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is

      function Get_Library_ALI_Dir_Callback
        (Project : GPR.Project_Id) return Path_Information;

      ----------------------------------
      -- Get_Library_ALI_Dir_Callback --
      ----------------------------------

      function Get_Library_ALI_Dir_Callback
        (Project : GPR.Project_Id) return Path_Information is
      begin
         return Project.Library_ALI_Dir;
      end Get_Library_ALI_Dir_Callback;

   begin
      return Get_Directory (Project,
                            Get_Library_ALI_Dir_Callback'Unrestricted_Access);
   end Library_Ali_Directory;

   ---------------------------
   -- For_Each_Project_Node --
   ---------------------------

   procedure For_Each_Project_Node
     (Tree     : GPR.Project_Node_Tree_Ref;
      Root     : Project_Node_Id;
      Callback : access procedure
                   (Tree : GPR.Project_Node_Tree_Ref; Node : Project_Node_Id))
   is
      use Project_Sets;
      Seen : Project_Sets.Set;

      procedure Process_Project (Proj : Project_Node_Id);

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Proj : Project_Node_Id) is
         With_Clause : Project_Node_Id := First_With_Clause_Of (Proj, Tree);
         Extended    : Project_Node_Id;
      begin
         if not Seen.Contains (Proj) then
            Seen.Include (Proj);

            Callback (Tree, Proj);

            while With_Clause /= Empty_Project_Node loop
               --  We have to ignore links back to the root project,
               --  which could only happen with "limited with", since
               --  otherwise the root project would not appear first in
               --  the topological sort, and then Start returns invalid
               --  results at least when its Recursive parameters is set
               --  to False.
               if Project_Node_Of (With_Clause, Tree) /= Root
                 and then not Is_Virtual_Extending
                   (Tree, Project_Node_Of (With_Clause, Tree))
               then
                  Process_Project (Project_Node_Of (With_Clause, Tree));
               end if;
               With_Clause := Next_With_Clause_Of (With_Clause, Tree);
            end loop;

            --  Is this an extending project ?

            Extended := Extended_Project_Of
              (Project_Declaration_Of (Proj, Tree), Tree);
            if Extended /= Empty_Project_Node then
               Process_Project (Extended);
            end if;
         end if;
      end Process_Project;

   begin
      Process_Project (Root);
   end For_Each_Project_Node;

   -------------------------------
   -- Compute_Imported_Projects --
   -------------------------------

   procedure Compute_Imported_Projects (Project : Project_Type'Class) is
   begin
      if Project.Data /= null
         and then Project.Data.Imported_Projects.Items = null
      then
         declare
            procedure Do_Add (T : GPR.Project_Node_Tree_Ref;
                              P : Project_Node_Id);

            procedure Do_Add
              (T : GPR.Project_Node_Tree_Ref; P : Project_Node_Id)
            is
               Path : constant Path_Name_Type := GPR.Tree.Path_Name_Of (P, T);
            begin
               Append (Project.Data.Imported_Projects, Path);
            end Do_Add;
         begin
            For_Each_Project_Node
              (Project.Data.Tree.Tree, Project.Data.Node,
               Do_Add'Unrestricted_Access);
         end;
      end if;
   end Compute_Imported_Projects;

   --------------------
   -- Start_Reversed --
   --------------------

   function Start_Reversed
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Project_Iterator
   is
      Iter       : Project_Iterator;

      Project_Paths : Path_Sets.Set;

      procedure Add_Project (Project : Project_Type'Class);
      --  Fills Project_Iterator with a list of projects. For each of
      --  aggregated project trees (if any) corresponding projects are put in
      --  the list in the same reversed topological order as for regular
      --  project. Aggregate project itself goes in front of corresponding
      --  aggregated projects.

      procedure Add_Project (Project : Project_Type'Class) is
         P          : Project_Type;
         Aggregated : Aggregated_Project_List;
         Iter_Inner : Inner_Project_Iterator;
      begin
         if Project.Get_View = GPR.No_Project then
            --  View has not been computed for this project.
            return;
         end if;

         if Is_Aggregate_Project (Project) then
            --  processing aggregated project hierarchies
            Aggregated := Project.Data.View.Aggregated_Projects;

            --  aggregate project goes first in reversed order.
            if
              Project_Paths.Find
                (Project_Path (Project).Display_Full_Name) =
                  Path_Sets.No_Element
            then
               Iter.Project_List.Append (Project_Type (Project));
               Project_Paths.Include
                 (Project_Path (Project).Display_Full_Name);
            end if;

            while Aggregated /= null loop

               P := Project_Type
                 (Project_From_Path (Project.Data.Tree, Aggregated.Path));

               if Direct_Only then

                  if
                    Project_Paths.Find
                      (Project_Path (P).Display_Full_Name) =
                        Path_Sets.No_Element
                  then
                     --  we only need projects that are not yet in the list
                     Iter.Project_List.Append (P);
                     Project_Paths.Include
                       (Project_Path (P).Display_Full_Name);
                  end if;
               else

                  Add_Project (P);
               end if;

               Aggregated := Aggregated.Next;
            end loop;
         end if;

         --  For the regular project (aggregated or root) do a full
         --  iteration placing projects in the list.
         Iter_Inner :=
           Start_Reversed
             (Root_Project     => Project,
              Recursive        => Recursive,
              Direct_Only      => Direct_Only,
              Include_Extended => Include_Extended);

         loop
            exit when Current (Iter_Inner) = No_Project;

            if
              Project_Paths.Find
                (Current (Iter_Inner).Project_Path.Display_Full_Name) =
                  Path_Sets.No_Element
            then
               --  we only need projects that are not yet in the list
               if
                 Is_Aggregate_Project (Current (Iter_Inner))
                 and then not Direct_Only
               then
                  Add_Project (Current (Iter_Inner));
               else
                  Iter.Project_List.Append (Current (Iter_Inner));
                  Project_Paths.Include
                    (Current (Iter_Inner).Project_Path.Display_Full_Name);
               end if;
            end if;

            Next (Iter_Inner);
         end loop;
      end Add_Project;

   begin
      Iter.Root := Root_Project;

      if not Recursive then
         Iter.Project_List.Append (Root_Project);
         Iter.Project_Idx := Iter.Project_List.First_Index;
         return Iter;
      end if;

      Add_Project (Root_Project);
      Project_Paths.Clear;
      Iter.Project_Idx := Iter.Project_List.First_Index;

      return Iter;
   end Start_Reversed;

   --------------------
   -- Start_Reversed --
   --------------------

   function Start_Reversed
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Inner_Project_Iterator
   is
      Iter : Inner_Project_Iterator;
   begin
      Assert (Me, Root_Project.Data /= null,
              "Start: Uninitialized project passed as argument");

      Compute_Imported_Projects (Root_Project);

      if Recursive then
         Iter := Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => True,
            Include_Extended => Include_Extended,
            Current    => Root_Project.Data.Imported_Projects.Items'First - 1);
         Next (Iter);
         return Iter;
      else
         --  Include_Extended is in fact ignored here, since we only ever
         --  return the root project.
         return Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,  --  irrelevant
            Include_Extended => Include_Extended,
            Current   => Root_Project.Data.Imported_Projects.Items'First);
      end if;
   end Start_Reversed;

   -----------
   -- Start --
   -----------

   function Start
     (Root_Project       : Project_Type;
      Recursive          : Boolean := True;
      Direct_Only        : Boolean := False;
      Include_Extended   : Boolean := True) return Project_Iterator
   is
      Iter       : Project_Iterator;

      Project_Paths : Path_Sets.Set;

      procedure Add_Project (Project : Project_Type'Class);
      --  Fills Project_Iterator with a list of projects. For each of
      --  aggregated project trees (if any) corresponding projects are put in
      --  the list in the same topological order as for regular project.
      --  Aggregate project itself goes after corresponding aggregated
      --  projects.

      procedure Add_Project (Project : Project_Type'Class) is
         P          : Project_Type;
         Aggregated : Aggregated_Project_List;
         Iter_Inner : Inner_Project_Iterator;
      begin
         if Project.Get_View = GPR.No_Project then
            --  View has not been computed for this project.
            return;
         end if;

         if Is_Aggregate_Project (Project) then
            --  processing aggregated project hierarchies
            Aggregated := Project.Data.View.Aggregated_Projects;
            while Aggregated /= null loop
               P := Project_Type
                 (Project_From_Path
                    (Project.Data.Tree_For_Map, Aggregated.Path));

               if not Project_Paths.Contains
                 (P.Project_Path.Display_Full_Name)
               then
                  if Direct_Only then
                     Project_Paths.Include (P.Project_Path.Display_Full_Name);
                     Iter.Project_List.Append (P);
                  else
                     Add_Project (P);
                  end if;
               end if;

               Aggregated := Aggregated.Next;
            end loop;

            --  aggregate project goes last in straight order
            if not Project_Paths.Contains
              (Project.Project_Path.Display_Full_Name)
            then
               Project_Paths.Include (Project.Project_Path.Display_Full_Name);
               Iter.Project_List.Append (Project_Type (Project));
            end if;

         end if;

         --  For the regular project (aggregated or root) do a full
         --  iteration placing projects in the list.
         Iter_Inner :=
           Start
             (Root_Project     => Project,
              Recursive        => Recursive,
              Direct_Only      => Direct_Only,
              Include_Extended => Include_Extended);

         loop
            P := Current (Iter_Inner);
            exit when P = No_Project;

            if not Project_Paths.Contains
              (P.Project_Path.Display_Full_Name)
            then
               Project_Paths.Include (P.Project_Path.Display_Full_Name);

               if Is_Aggregate_Project (P) and then not Direct_Only then
                  --  aggregate library
                  Add_Project (P);
               else
                  Iter.Project_List.Append (P);
               end if;
            end if;

            Next (Iter_Inner);
         end loop;
      end Add_Project;

   begin
      Iter.Root := Root_Project;

      if not Recursive then
         Iter.Project_List.Append (Root_Project);
         Iter.Project_Idx := Iter.Project_List.First_Index;
         return Iter;
      end if;

      Add_Project (Root_Project);
      Project_Paths.Clear;
      Iter.Project_Idx := Iter.Project_List.First_Index;

      return Iter;
   end Start;

   -----------
   -- Start --
   -----------

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Inner_Project_Iterator
   is
      Iter : Inner_Project_Iterator;
   begin
      Compute_Imported_Projects (Root_Project);

      if Recursive then
         Iter := Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,
            Include_Extended => Include_Extended,
            Current   => Root_Project.Data.Imported_Projects.Last + 1);
         Next (Iter);
         return Iter;
      else
         return Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,  --  irrelevant
            Include_Extended => Include_Extended,
            Current  => Root_Project.Data.Imported_Projects.Items'First);
      end if;
   end Start;

   ---------------------
   -- Project_Imports --
   ---------------------

   procedure Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type'Class;
      Include_Extended : Boolean := False;
      Imports          : out Boolean;
      Is_Limited_With  : out Boolean)
   is
      With_Clause : Project_Node_Id;
      Extended    : Project_Node_Id;
      T           : constant GPR.Project_Node_Tree_Ref :=
                      Parent.Data.Tree.Tree;
   begin
      Assert (Me, Child.Data /= null, "Project_Imports: no child provided");

      if Parent = No_Project then
         Imports := True;
         Is_Limited_With := False;
         return;
      end if;

      With_Clause := First_With_Clause_Of (Parent.Data.Node, T);

      while With_Clause /= Empty_Project_Node loop
         --  We cannot compare the nodes directly, since they might be the same
         --  in two aggregated projects, even when this is not the same project

         if Get_Name_String
           (Path_Name_Of (Project_Node_Of (With_Clause, T), T)) =
             Child.Project_Path.Display_Full_Name
         then
            Imports         := True;
            Is_Limited_With := Non_Limited_Project_Node_Of (With_Clause, T)
              = Empty_Project_Node;
            return;
         end if;

         With_Clause := Next_With_Clause_Of (With_Clause, T);
      end loop;

      --  Handling for extending projects ?

      if Include_Extended then
         Extended := Extended_Project_Of
           (Project_Declaration_Of (Parent.Data.Node, T), T);
         if Extended = Child.Data.Node then
            Imports := True;
            Is_Limited_With := False;
            return;
         end if;
      end if;

      --  Handling aggregate libraries
      if Is_Aggregate_Library (Parent) then
         Is_Limited_With := False;
         declare
            Aggregated : Aggregated_Project_List :=
              Parent.Data.View.Aggregated_Projects;
            P          : Project_Type;
         begin
            while Aggregated /= null loop
               P := Project_Type
                 (Project_From_Path (Parent.Data.Tree, Aggregated.Path));

               if P.Data = Child.Data then
                  Imports := True;
                  return;
               end if;

               Aggregated := Aggregated.Next;
            end loop;
         end;
      end if;

      Imports := False;
      Is_Limited_With := False;
   end Project_Imports;

   --------------------------------
   -- Compute_Importing_Projects --
   --------------------------------

   procedure Compute_Importing_Projects
     (Project      : Project_Type'Class;
      Root_Project : Project_Type'Class)
   is
      type Boolean_Array is array (Positive range <>) of Boolean;

      All_Prj      : Path_Name_Id_Array_Access :=
        Root_Project.Data.Imported_Projects.Items;
      All_Prj_Last : Integer := Root_Project.Data.Imported_Projects.Last;
      Importing    : Path_Name_Id_Array_Access;
      Index        : Integer;
      Parent       : Project_Type;
      Imports, Is_Limited_With : Boolean;

      procedure Merge_Project (P : Project_Type; Inc : in out Boolean_Array);
      --  Merge the imported projects of P with the ones for Project

      -------------------
      -- Merge_Project --
      -------------------

      procedure Merge_Project
        (P : Project_Type; Inc : in out Boolean_Array)
      is
         Index2 : Integer;
      begin
         for J in P.Data.Importing_Projects'Range loop
            Index2 := All_Prj'First;
            while All_Prj (Index2) /= P.Data.Importing_Projects (J) loop
               Index2 := Index2 + 1;
            end loop;

            Inc (Index2) := True;
         end loop;
      end Merge_Project;

   begin
      if Project.Data.Importing_Projects /= null then
         return;
      end if;

      --  Prevent a recursive call to this procedure: if the project has
      --  a "limited with", we could end up calling Compute_Importing_Project
      --  again for the same project, thus an infinite loop. To prevent this,
      --  we set Dummy. That means however that we will not correctly compute
      --  the list of imported project for imported projects below, so we
      --  should not store them.

      Project.Data.Importing_Projects :=
        Unknown_Importing_Projects'Unrestricted_Access;

      if All_Prj = null then
         Compute_Imported_Projects (Root_Project);
         All_Prj := Root_Project.Data.Imported_Projects.Items;
         All_Prj_Last := Root_Project.Data.Imported_Projects.Last;
      end if;

      --  We consider that an extending project is "importing" its
      --  extended project, since it relies on it.

      declare
         Include   : Boolean_Array (1 .. All_Prj_Last) := (others => False);
         Was_Unknown : Boolean;

      begin
         for Index in Include'Range loop
            Parent := Project_Type
              (Project_From_Path (Project.Data.Tree, All_Prj (Index)));

            --  Avoid processing a project twice

            if not Include (Index)
              and then Parent /= Project_Type (Project)
            then
               Project_Imports
                 (Parent, Child => Project,
                  Include_Extended => True,
                  Imports         => Imports,
                  Is_Limited_With => Is_Limited_With);

               if Imports then
                  Include (Index) := True;

                  --  The list computed for Parent might be incorrect is
                  --  somewhere there is a "limited with" that goes back to
                  --  Project (since we have set a Dummy above to prevent
                  --  infinite recursion). So we will reset the list to
                  --  null below, which means we might end up recomputing
                  --  it later.

                  Was_Unknown := Parent.Data.Importing_Projects = null
                    or else Parent.Data.Importing_Projects.all'Address =
                      Unknown_Importing_Projects'Address;

                  Compute_Importing_Projects (Parent, Root_Project);
                  Merge_Project (Parent, Include);

                  if Was_Unknown then
                     --  We cannot rely on the computed value if the parent
                     --  was also importing Project, so we must reset the cache
                     --  in that case. Otherwise keep the cache for maximum
                     --  efficiency
                     for J in Parent.Data.Importing_Projects'Range loop
                        if Parent.Data.Importing_Projects (J) =
                          Get_View (Project).Path.Name
                        then
                           Unchecked_Free (Parent.Data.Importing_Projects);
                           exit;
                        end if;
                     end loop;
                  end if;
               end if;
            end if;
         end loop;

         --  Done processing everything

         Index := 0;
         for Inc in Include'Range loop
            if Include (Inc) then
               Index := Index + 1;
            end if;
         end loop;

         --  Keep the last place for the project itself

         Importing := new Path_Name_Id_Array (1 .. Index + 1);

         Index := Importing'First;
         for Inc in Include'Range loop
            if Include (Inc) then
               Importing (Index) := All_Prj (Inc);
               Index := Index + 1;
            end if;
         end loop;
      end;

      Importing (Importing'Last) := GPR.Tree.Path_Name_Of
        (Project.Data.Node, Project.Data.Tree.Tree);
      Project.Data.Importing_Projects := Importing;

      --  The code below is used for debugging

      if Active (Debug) then
         Trace (Debug, "Find_All_Projects_Importing: " & Project.Name);
         for J in Project.Data.Importing_Projects'Range loop
            Trace (Debug, Get_String (Project.Data.Importing_Projects (J)));
         end loop;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         if Project.Data.Importing_Projects.all'Address /=
            Unknown_Importing_Projects'Address
         then
            Unchecked_Free (Project.Data.Importing_Projects);
         end if;
         Project.Data.Importing_Projects := null;
   end Compute_Importing_Projects;

   ---------------------------------
   -- Find_All_Projects_Importing --
   ---------------------------------

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Project_Iterator
   is
      Iter, Cleanup_Iter : Project_Iterator;
      Iter_Inner         : Inner_Project_Iterator;

      Local_Roots : Project_Lists.Vector := Project_Lists.Empty_Vector;

      Project_Paths : Path_Sets.Set := Path_Sets.Empty_Set;

      procedure Add_Local_Roots (Project : Project_Type);
      --  creating a list of root level aggregated projects

      procedure Add_Local_Roots (Project : Project_Type) is
         P          : Project_Type;
         Aggregated : Aggregated_Project_List;
      begin

         if Is_Aggregate_Project (Project) then
            Aggregated := Project.Data.View.Aggregated_Projects;

            while Aggregated /= null loop

               P := Project_Type
                 (Project_From_Path (Project.Data.Tree, Aggregated.Path));

               Add_Local_Roots (P);

               Aggregated := Aggregated.Next;
            end loop;
         else

            Local_Roots.Append (Project);
         end if;
      end Add_Local_Roots;
   begin
      Iter.Root      := Project;
      Iter.Importing := True;

      if Is_Aggregate_Project (Project.Data.Tree_For_Map.Root) then
         --  We need to look for importing projects in all trees created for
         --  each  directly aggregated project.
         Add_Local_Roots (Project.Data.Tree_For_Map.Root);

         for I in Local_Roots.First_Index .. Local_Roots.Last_Index loop

            Iter_Inner := Find_All_Projects_Importing
              (Project      => Project,
               Root_Project => Local_Roots.Element (I),
               Include_Self => Include_Self,
               Direct_Only  => Direct_Only);

            loop
               exit when Current (Iter_Inner) = No_Project;

               if
                 not Project_Paths.Contains
                   (Current (Iter_Inner).Project_Path.Display_Full_Name)
               then
                  --  avoiding possible duplication
                  Iter.Project_List.Append (Current (Iter_Inner));
                  Project_Paths.Include
                    (Current (Iter_Inner).Project_Path.Display_Full_Name);
               end if;

               Next (Iter_Inner);
            end loop;

            --  We need to reset importing projects for each local root
            --  and the project in question before the next pass.
            Unchecked_Free (Project.Data.Importing_Projects);
            Cleanup_Iter := Start (Local_Roots (I));
            while Current (Cleanup_Iter) /= No_Project loop
               Unchecked_Free (Current (Cleanup_Iter).Data.Importing_Projects);
               Next (Cleanup_Iter);
            end loop;
         end loop;

         Iter.Project_Idx := Iter.Project_List.First_Index;
      end if;

      Iter_Inner := Find_All_Projects_Importing
        (Project      => Project,
         Root_Project => Project.Data.Tree_For_Map.Root,
         Include_Self => Include_Self,
         Direct_Only  => Direct_Only);

      loop
         exit when Current (Iter_Inner) = No_Project;

         if not Project_Paths.Contains
           (Current (Iter_Inner).Project_Path.Display_Full_Name)
         then
            Project_Paths.Include
              (Current (Iter_Inner).Project_Path.Display_Full_Name);
            Iter.Project_List.Append (Current (Iter_Inner));
         end if;
         Next (Iter_Inner);
      end loop;

      --  Again, we need to clean up all stored Importing_Projects, otherwise
      --  if somewhere in the hierarchy there is an aggregate/aggregate library
      --  project, the stored info is not correct.
      Cleanup_Iter := Start (Project.Data.Tree_For_Map.Root);
      while Current (Cleanup_Iter) /= No_Project loop
         Unchecked_Free (Current (Cleanup_Iter).Data.Importing_Projects);
         Next (Cleanup_Iter);
      end loop;

      Iter.Project_Idx := Iter.Project_List.First_Index;
      Project_Paths.Clear;
      return Iter;
   end Find_All_Projects_Importing;

   ---------------------------------
   -- Find_All_Projects_Importing --
   ---------------------------------

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Root_Project : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Inner_Project_Iterator
   is
      Iter         : Inner_Project_Iterator;
   begin
      if Project = No_Project then
         return Start (Root_Project, Recursive => True);
      end if;

      Trace (Me, "Find_All_Projects_Importing " & Project.Name
         & " with root=" & Root_Project.Name);

      Compute_Imported_Projects (Root_Project);
      Compute_Importing_Projects (Project, Root_Project);

      Iter := Inner_Project_Iterator'
        (Root             => Project,
         Direct_Only      => Direct_Only,
         Importing        => True,
         Reversed         => False,
         Include_Extended => True,   --  ??? Should this be configurable
         Current          => Project.Data.Importing_Projects'Last + 1);

      --  The project itself is always at index 'Last
      if not Include_Self then
         Iter.Current := Iter.Current - 1;
      end if;

      Next (Iter);
      return Iter;
   end Find_All_Projects_Importing;

   -------------
   -- Current --
   -------------

   function Current
     (Iterator : Project_Iterator) return Project_Type
   is
   begin
      if Iterator.Project_List.To_Cursor (Iterator.Project_Idx) =
        Project_Lists.No_Element
      then
         return No_Project;
      end if;

      return Iterator.Project_List.Element (Iterator.Project_Idx);
   end Current;

   -------------
   -- Current --
   -------------

   function Current
     (Iterator : Inner_Project_Iterator) return Project_Type
   is
      P : Path_Name_Type;
   begin
      if Iterator.Importing then
         if Iterator.Current >=
           Iterator.Root.Data.Importing_Projects'First
         then
            return Project_Type
              (Project_From_Path
                 (Iterator.Root.Data.Tree_For_Map,
                  Iterator.Root.Data.Importing_Projects (Iterator.Current)));
         end if;

      elsif Iterator.Current >=
        Iterator.Root.Data.Imported_Projects.Items'First
        and then Iterator.Current <=
          Iterator.Root.Data.Imported_Projects.Last
      then
         P := Iterator.Root.Data.Imported_Projects.Items (Iterator.Current);
         return Project_Type
           (Project_From_Path (Iterator.Root.Data.Tree_For_Map, P));
      end if;

      return No_Project;
   end Current;

   ---------------------
   -- Is_Limited_With --
   ---------------------

   function Is_Limited_With (Iterator : Project_Iterator) return Boolean
   is
      Imports, Is_Limited_With : Boolean;
   begin

      if Iterator.Importing then

         if Is_Aggregate_Project (Iterator.Root) then
            --  aggregate projects cannot be imported
            return False;
         end if;

         Project_Imports
           (Current (Iterator), Iterator.Root,
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);
      else
         Project_Imports
           (Iterator.Root, Current (Iterator),
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);
      end if;

      return Imports and Is_Limited_With;

   end Is_Limited_With;

   ---------------------
   -- Is_Limited_With --
   ---------------------

   function Is_Limited_With
     (Iterator : Inner_Project_Iterator) return Boolean
   is
      Imports, Is_Limited_With : Boolean;
   begin
      if Iterator.Importing then
         Project_Imports
           (Current (Iterator), Iterator.Root,
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);

      else
         Project_Imports
           (Iterator.Root, Current (Iterator),
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);
      end if;

      return Imports and Is_Limited_With;
   end Is_Limited_With;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Project_Iterator) is
   begin
      Iterator.Project_Idx := Iterator.Project_Idx + 1;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Inner_Project_Iterator) is
      Imports, Is_Limited_With : Boolean;
   begin
      if Iterator.Reversed then
         Iterator.Current := Iterator.Current + 1;

         if Iterator.Direct_Only then
            if Iterator.Importing then
               while Iterator.Current <=
                 Iterator.Root.Data.Importing_Projects'Last
               loop
                  Project_Imports
                    (Current (Iterator), Iterator.Root,
                     Iterator.Include_Extended,
                     Imports => Imports, Is_Limited_With => Is_Limited_With);
                  exit when Imports;
                  Iterator.Current := Iterator.Current + 1;
               end loop;

            else
               while Iterator.Current <=
                 Iterator.Root.Data.Imported_Projects.Last
               loop
                  Project_Imports
                    (Iterator.Root, Current (Iterator),
                     Iterator.Include_Extended,
                     Imports => Imports, Is_Limited_With => Is_Limited_With);
                  exit when Imports;
                  Iterator.Current := Iterator.Current + 1;
               end loop;
            end if;
         end if;

      else
         Iterator.Current := Iterator.Current - 1;

         if Iterator.Direct_Only then
            if Iterator.Importing then
               while Iterator.Current >=
                 Iterator.Root.Data.Importing_Projects'First
               loop
                  Project_Imports
                    (Current (Iterator), Iterator.Root,
                     Iterator.Include_Extended,
                     Imports => Imports, Is_Limited_With => Is_Limited_With);
                  exit when Imports;
                  Iterator.Current := Iterator.Current - 1;
               end loop;

            else
               while Iterator.Current >=
                 Iterator.Root.Data.Imported_Projects.Items'First
               loop
                  Project_Imports
                    (Iterator.Root, Current (Iterator),
                     Iterator.Include_Extended,
                     Imports => Imports, Is_Limited_With => Is_Limited_With);
                  exit when Imports;
                  Iterator.Current := Iterator.Current - 1;
               end loop;
            end if;
         end if;
      end if;
   end Next;

   --------------------------------
   -- Compute_Scenario_Variables --
   --------------------------------

   procedure Compute_Scenario_Variables
     (Tree      : Project_Tree_Data_Access;
      Recursive : Boolean := True;
      Errors    : Error_Report := null)
   is
      Typed_List   : Scenario_Variable_Array_Access;
      Untyped_List : Untyped_Variable_Array_Access;
      T_Curr  : Positive;
      U_Curr  : Positive;

      T_Curr2 : Natural;

      Var_Quantity : Natural;

      package Name_Id_Sets is new Ada.Containers.Ordered_Sets (GPR.Name_Id);
      Inconsistent_SC_Externals : Name_Id_Sets.Set := Name_Id_Sets.Empty_Set;

      function Count_Vars return Natural;
      --  Return the number of scenario variables in tree

      function Not_Already
           (UVs      : Untyped_Variable_Array_Access;
            Last     : Positive;
            Ext_Name : GPR.Name_Id) return Boolean;
         --  Checks that an untyped variable with same name
         --  has not been registered yet.

      procedure Register_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type);
      --  Wrapper that calls either Register_Scenario_Var or
      --  Register_Untyped_Var depending on the kind of the variable.

      procedure Register_Scenario_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type;
         Errors   : Error_Report := null);
      --  Add the variable to the list of scenario variables, if not there yet
      --  (see the documentation for Scenario_Variables for the exact rules
      --  used to detect aliases).

      procedure Register_Untyped_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type);
      --  Likewise, add the variable to the list of untyped variables.

      function External_Default
        (Project     : Project_Type;
         Var         : Project_Node_Id;
         Pkg         : Project_Node_Id;
         T           : GPR.Project_Node_Tree_Ref;
         Nested_Expr : Project_Node_Id := Empty_Project_Node)
         return Name_Id;
      --  Return the default value for the variable. Var must be a variable
      --  declaration or a variable reference. This routine supports only
      --  all kinds of expressions, but for composite values it will set on
      --  the Uses_Variables flag for the root project.
      --  Expr is only used for nested external references in the variable
      --  declaration to evaluate the proper expression.

      ----------------
      -- Count_Vars --
      ----------------

      function Count_Vars return Natural is
         Count : Natural := 0;

         procedure Cb
           (Variable : Project_Node_Id;
            Prj      : Project_Node_Id;
            Pkg      : Project_Node_Id;
            Project  : Project_Type);
         --  Increment the total number of variables

         --------
         -- Cb --
         --------

         procedure Cb
           (Variable : Project_Node_Id;
            Prj      : Project_Node_Id;
            Pkg      : Project_Node_Id;
            Project  : Project_Type)
         is
            pragma Unreferenced (Prj, Pkg);
            Node_Tree : constant  GPR.Project_Node_Tree_Ref :=
              Project.Data.Tree.Tree;
            Expr : Project_Node_Id := Expression_Of (Variable, Node_Tree);
         begin
            if Expression_Kind_Of (Variable, Node_Tree) = List then
               --  Extenal_As_list case. Just count as one and ignore the rest
               --  of expression.
               Count := Count + 1;
               return;
            end if;

            while Expr /= Empty_Project_Node loop

               Expr := First_Term (Expr, Node_Tree);
               if Next_Term (Expr, Node_Tree) /= Empty_Project_Node then
                  --  Non-canonical nesting, we do not care.
                  return;
               end if;

               Expr := Current_Term (Expr, Node_Tree);

               if Kind_Of (Expr, Node_Tree) = N_External_Value then
                  Count := Count + 1;
                  --  That is nesting, we need to iterate deeper.
                  Expr := External_Default_Of (Expr, Node_Tree);
               else
                  --  End of nesting.
                  return;
               end if;
            end loop;
         end Cb;

      begin
         For_Each_External_Variable_Declaration
           (Tree.Root, Recursive => Recursive,
            Callback => Cb'Unrestricted_Access);
         return Count;
      end Count_Vars;

      ----------------------
      -- External_Default --
      ----------------------

      function External_Default
        (Project     : Project_Type;
         Var         : Project_Node_Id;
         Pkg         : Project_Node_Id;
         T           : GPR.Project_Node_Tree_Ref;
         Nested_Expr : Project_Node_Id := Empty_Project_Node)
         return Name_Id
      is
         V : Variable_Value;

         Name : constant String :=
           Get_Name_String (GPR.Tree.Name_Of (Var, T));
         --  For diagnostic purposes.

         Proj : Project_Type    := Tree.Root;
         Expr : Project_Node_Id :=
           (if Nested_Expr = Empty_Project_Node then
               Expression_Of (Var, T)
            else
               Nested_Expr);

         procedure Check_Complexity (Expression : Project_Node_Id);
         --  Check whether or not the default value is a simple one,
         --  and mark project tree not editable, if the value is complex.

         procedure Check_Complexity (Expression : Project_Node_Id) is
            Expr : Project_Node_Id := Expression;
         begin

            if Kind_Of (Expr, T) /= N_Literal_String then
               Expr := First_Term (Expr, T);
               if Next_Term (Expr, T) /= Empty_Project_Node then
                  Trace (Me, "No project editing: "
                         & "Default value cannot be a concatenation");
                  Proj.Data.Uses_Variables := True;  --  Prevent edition
                  return;
               end if;

               Expr := Current_Term (Expr, T);

               if Kind_Of (Expr, T) = N_Variable_Reference then
               --  A variable reference, look for the corresponding string
               --  literal.

                  declare
                     Var    : constant Name_Id :=
                       GPR.Tree.Name_Of (Expr, T);
                     In_Prj : constant Project_Node_Id :=
                       Project_Node_Of (Expr, T);
                     Decl   : Project_Node_Id;
                  begin
                     if In_Prj /= Empty_Project_Node then
                        --  This variable is defined in another project, get
                        --  project reference.
                        Proj := Project_Type
                          (Project_From_Name
                             (Tree, GPR.Tree.Name_Of (In_Prj, T)));
                     else
                        Proj := Project;
                     end if;

                     --  Look for Var declaration into the project

                     Decl := First_Declarative_Item_Of
                       (Project_Declaration_Of (Proj.Data.Node, T), T);

                     while Decl /= Empty_Project_Node loop
                        Expr := Current_Item_Node (Decl, T);

                        if GPR.Tree.Name_Of (Expr, T) = Var then
                           Expr := Expression_Of (Expr, T);
                           Expr := First_Term (Expr, T);
                           --  Get expression and corresponding term

                           --  Check now that this is not a composite value

                           if Next_Term (Expr, T) /= Empty_Project_Node then
                              Trace
                                (Me, "No project editing: "
                                 & "Default value cannot be a concatenation");
                              Proj.Data.Uses_Variables := True;
                              --  Prevent edition
                              return;
                           end if;

                           --  Get the string literal

                           Expr := Current_Term (Expr, T);
                           exit;
                        end if;
                        Decl := Next_Declarative_Item (Decl, T);
                     end loop;
                  end;
               end if;

               if Kind_Of (Expr, T) /= N_Literal_String then
                  Trace (Me,  "No project editing: "
                         & "Default value can only be literal string");
                  Proj.Data.Uses_Variables := True; --  prevent edition
                  return;
               end if;
            end if;
         end Check_Complexity;

         The_Name        : Name_Id     := No_Name;
         The_Package     : Package_Id  := No_Package;
      begin
         Expr := First_Term   (Expr, T);
         Expr := Current_Term (Expr, T);

         if Kind_Of (Expr, T) /= N_External_Value then
            return No_Name;
         end if;

         Expr := External_Default_Of (Expr, T);

         if Expr = Empty_Project_Node then
            return No_Name;
         end if;

         Check_Complexity (Expr);

         The_Name := GPR.Tree.Name_Of (Pkg, T);

         The_Package := Project.Get_View.Decl.Packages;
         while The_Package /= No_Package
           and then
             Project.Tree_View.Shared.Packages.Table (The_Package).Name /=
           The_Name
         loop
            The_Package :=
              Project.Tree_View.Shared.Packages.Table (The_Package).Next;
         end loop;

         if Active (Me_SV) then
            if Nested_Expr = Empty_Project_Node then
               Trace (Me_SV, "We will try to compute default of:");
            else
               Trace
                 (Me_SV,
                  "We will try to compute default "
                  & "of a nested sub-expression from:");
            end if;
            Pretty_Print
              (Var, T, Backward_Compatibility => False);
         end if;

         V := GPR.Proc.Expression
           (Project                => Project.Data.View,
            Shared                 => Project.Tree_View.Shared,
            From_Project_Node      => Project.Node,
            From_Project_Node_Tree => T,
            Env                    => Project.Data.Tree.Env.Env,
            Pkg                    => The_Package,
            First_Term             =>
              First_Term
                (Expr, T),
            Kind                    =>
              Expression_Kind_Of (Expr, T));
         Trace (Me_SV, "Value is: " & Get_Name_String (V.Value));
         return V.Value;
      exception
         when Ex : others =>
            Trace
              (Me_SV, "Error when computing default for "
               & Name & " from project " & Project.Name & ":");
            Trace
              (Me_SV, Exception_Information (Ex));
            return GPR.No_Name;
      end External_Default;

      -----------------
      -- Not_Already --
      -----------------

      function Not_Already
        (UVs      : Untyped_Variable_Array_Access;
         Last     : Positive;
         Ext_Name : GPR.Name_Id) return Boolean
      is
      begin
         for I in 1 .. Last - 1 loop
            if UVs (I).Name = Ext_Name then
               return False;
            end if;
         end loop;
         return True;
      end Not_Already;

      ------------------
      -- Register_Var --
      ------------------

      procedure Register_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type)
      is
         T : constant  GPR.Project_Node_Tree_Ref :=
           Project.Data.Tree.Tree;

         function Is_Simple_Scenario_Variable return Boolean;
         --  Check whether or not given variable is a simple canonical
         --  Scenario Variable, that is there are no concatenations in the
         --  default value or after the external variable declaration
         --  and so on.

         function Is_Simple_Scenario_Variable return Boolean is

            Expr : Project_Node_Id;
         begin
            Expr :=
              First_Term (Expression_Of (Variable, T), T);

            if Next_Term (Expr, T) /= Empty_Project_Node then
               --  Not good, we have a declaration of the following kind:
               --  Val : Type := External ("Ext", "default") & <something>
               return False;
            end if;

            Expr := Expression_Of (Variable, T);
            Expr := First_Term   (Expr, T);
            Expr := Current_Term (Expr, T);
            Expr := External_Default_Of (Expr, T);

            if Expr /= Empty_Project_Node and then
              Kind_Of (Expr, T) /= N_Literal_String
            then
               Expr := First_Term (Expr, T);
               if Next_Term (Expr, T) /= Empty_Project_Node then
                  --  Not good, we have a declaration of the following kind:
                  --  Val : Type := External ("Ext", "default" & <something>)
                  return False;
               end if;
            end if;

            return True;
         end Is_Simple_Scenario_Variable;

      begin
         Trace
           (Me_SV, "Project: " & Project.Project_Path.Display_Full_Name);
         case Kind_Of (Variable, T) is

            when N_Variable_Declaration =>
               Register_Untyped_Var (Variable, Proj, Pkg, Project);

            when N_Typed_Variable_Declaration =>
               if Is_Simple_Scenario_Variable then
                  Register_Scenario_Var
                    (Variable, Proj, Pkg, Project, Errors);
               else
                  Register_Untyped_Var (Variable, Proj, Pkg, Project);
               end if;
            when others =>
               Trace (Me, "Unexpected kind of variable");
         end case;
      end Register_Var;

      ---------------------------
      -- Register_Scenario_Var --
      ---------------------------

      procedure Register_Scenario_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type;
         Errors   : Error_Report := null)
      is
         pragma Unreferenced (Proj, Errors);
         T : constant  GPR.Project_Node_Tree_Ref :=
           Project.Data.Tree.Tree;

         V        : constant Name_Id :=
           External_Reference_Of (Variable, T);
         N        : constant String := Get_String (V);
         Var      : Scenario_Variable;

         Is_Valid, Duplicate_Found : Boolean;

         function "<" (L, R : String_Access) return Boolean is (L.all < R.all);
         procedure Sort_Values is new
           Ada.Containers.Generic_Array_Sort
             (Positive, String_Access, String_List);

         procedure Look_For_Duplicate_SVs
           (Ext_Ref_Name :     String;
            Found        : out Boolean);
         --  Compare current Var with all already stored Scenario Variables
         --  and if found check that they have same set of possible values.

         procedure Look_For_Duplicate_SVs
           (Ext_Ref_Name :     String;
            Found        : out Boolean)
         is
            Old_Var : Scenario_Variable := No_Variable;

            Dummy : Project_Tree;
            --  Possible_Values_Of doesn't reference the Tree parameter
            --  that has been left only for compatibility.
         begin
            for Index in 1 .. T_Curr - 1 loop
               if External_Name (Typed_List (Index)) = Ext_Ref_Name then
                  Trace (Me_SV, "Same external already registered,"
                         & " comparing set of possible values");
                  Old_Var := Typed_List (Index);

                  declare
                     Old_Values : String_List_Access :=
                       new String_List'(Possible_Values_Of (Dummy, Old_Var));
                     New_Values : String_List_Access :=
                       new String_List'(Possible_Values_Of (Dummy, Var));

                     Values_Identical : Boolean := True;
                  begin
                     if Old_Values.all'Length /= New_Values.all'Length then
                        Trace (Me_SV, "different amount of values");
                        Values_Identical := False;
                     else
                        Sort_Values (Old_Values.all);
                        Sort_Values (New_Values.all);
                        for I in Old_Values'Range loop
                           if Old_Values (I).all /= New_Values (I).all then
                              Trace
                                (Me_SV,
                                 "Unmatched values: " & Old_Values (I).all
                                 & " and " & New_Values (I).all);
                              Values_Identical := False;
                              exit;
                           end if;
                        end loop;
                     end if;

                     Free (Old_Values);
                     Free (New_Values);

                     if not Values_Identical then
                        if
                          Old_Var.First_Project_Path = Var.First_Project_Path
                        then
                           --  Same project
                           Trace (Me_SV,
                                  Project.Project_Path.Display_Full_Name
                                   & ": Scenario variables "
                                   & Get_Name_String (Old_Var.Var_Name)
                                   & " and "
                                   & Get_Name_String (Var.Var_Name)
                                   & " controlled by same external "
                                   & Ext_Ref_Name
                                   & " have different sets of possible values"
                                   & ASCII.LF);
                        else
                           --  Aggregated projects with same name
                           Trace
                             (Me_SV,
                              "Scenario variables "
                               & Get_Name_String (Old_Var.First_Project_Path)
                               & ": "
                               & Get_Name_String (Old_Var.Var_Name)
                               & " and "
                               & Project.Project_Path.Display_Full_Name
                               & ": "
                               & Get_Name_String (Var.Var_Name)
                               & " controlled by same external "
                               & Ext_Ref_Name
                               & " have different sets of possible values"
                               & ASCII.LF);
                        end if;

                        Inconsistent_SC_Externals.Include (Old_Var.Ext_Name);
                     end if;
                  end;

                  Found := True;
                  return;
               end if;
            end loop;

            Found := False;
         end Look_For_Duplicate_SVs;

      begin
         Trace
           (Me_SV, "Register_Scenario_Var " &
              Get_Name_String (GPR.Tree.Name_Of (Variable, T)));

         Var := Scenario_Variable'
           (Ext_Name        => V,
            Var_Name    => GPR.Tree.Name_Of (Variable, T),
            Default     => External_Default (Project, Variable, Pkg, T),
            String_Type => String_Type_Of (Variable, T),
            Tree_Ref    => T,
            Value       => GPR.Ext.Value_Of
              (Tree.Env.Env.External, V,
               With_Default =>
                 External_Default (Project, Variable, Pkg, T)),
            First_Project_Path => Project.Data.View.Path.Display_Name);

         Look_For_Duplicate_SVs (N, Duplicate_Found);
         if Duplicate_Found then
            --  Nothing to add for the root one, however there may be some new
            --  nested ones.
            goto Unwind;
         end if;

         Typed_List (T_Curr) := Var;

         --  Ensure the external reference actually exists and has a valid
         --  value.

         Is_Valid := GPR.Ext.Value_Of
            (Tree.Env.Env.External, Var.Ext_Name) /= No_Name;

         if Is_Valid then
            declare
               Current : constant Name_Id :=
                  GPR.Ext.Value_Of (Tree.Env.Env.External, Var.Ext_Name);
               Iter : String_List_Iterator := Value_Of (T, Var);
            begin
               Is_Valid := False;

               while not Done (Iter) loop
                  if Data (T, Iter) = Current then
                     Is_Valid := True;
                     exit;
                  end if;
                  Iter := Next (T, Iter);
               end loop;
            end;
         end if;

         if not Is_Valid then
            if Var.Default /= No_Name then
               GPR.Ext.Add
                  (Tree.Env.Env.External, N, Get_Name_String (Var.Default),
                   GPR.Ext.From_Command_Line);
            else
               GPR.Ext.Add
                 (Tree.Env.Env.External, N,
                  Get_Name_String
                     (String_Value_Of
                        (First_Literal_String (Var.String_Type, T), T)),
                  GPR.Ext.From_Command_Line);
            end if;
         end if;

         T_Curr := T_Curr + 1;

         <<Unwind>>
         --  Unwinding nested external references if any.
         if Active (Me_SV) then
            Increase_Indent (Me_SV, "Unwind nested external references");
         end if;

         declare
            Expression : Project_Node_Id;

            Expr       : Project_Node_Id :=
              Expression_Of (Variable, T);
            Ref        : Name_Id;
         begin
            Expr :=
              External_Default_Of
                (Current_Term
                   (First_Term
                      (Expr, T),
                    T),
                 T);
            Expression := Expr;
            while Expr /= Empty_Project_Node loop
               Expr := First_Term (Expr, T);
               if Next_Term (Expr, T) /= Empty_Project_Node then
                  if Active (Me_SV) then
                     Decrease_Indent
                       (Me_SV, "Unwind terminated: Not canonical nesting");
                  end if;

                  return;
               end if;

               Expr := Current_Term (Expr, T);
               if Kind_Of (Expr, T) = N_External_Value then
                  Ref := String_Value_Of (External_Reference_Of (Expr, T), T);
                  Trace (Me_SV,
                         "Nested external reference: "
                         & Get_Name_String (Ref));

                  Look_For_Duplicate_SVs
                    (Get_Name_String (Ref), Duplicate_Found);
                  if not Duplicate_Found then

                     Var.Ext_Name := Ref;
                     Var.Default :=
                       External_Default
                         (Project, Variable, Pkg, T, Expression);

                     Typed_List (T_Curr) := Var;
                     T_Curr := T_Curr + 1;
                  end if;

                  Expr := External_Default_Of (Expr, T);
                  Expression := Expr;
               else
                  if Active (Me_SV) then
                     Decrease_Indent (Me_SV, "Unwind finished");
                  end if;

                  return;
               end if;
            end loop;
         end;
      end Register_Scenario_Var;

      --------------------------
      -- Register_Untyped_Var --
      --------------------------

      procedure Register_Untyped_Var
        (Variable : Project_Node_Id;
         Proj     : Project_Node_Id;
         Pkg      : Project_Node_Id;
         Project  : Project_Type)
      is
         pragma Unreferenced (Proj);
         T : constant  GPR.Project_Node_Tree_Ref :=
           Project.Data.Tree.Tree;

         V        : constant Name_Id := External_Reference_Of (Variable, T);
         N        : constant String := Get_String (V);
         Var      : Untyped_Variable;
      begin
         Trace
           (Me_SV, "Register_Untyped_Var " &
              Get_Name_String (GPR.Tree.Name_Of (Variable, T)));

         for Index in 1 .. U_Curr - 1 loop
            if External_Name (Untyped_List (Index)) = N then
               --  Nothing to do
               return;
            end if;
         end loop;

         Var := Untyped_Variable'
           (Name    => V,
            Default => External_Default (Project, Variable, Pkg, T),
            Value   => GPR.Ext.Value_Of
              (Tree.Env.Env.External, V,
               With_Default =>
                 External_Default (Project, Variable, Pkg, T)));

         Untyped_List (U_Curr) := Var;

         U_Curr := U_Curr + 1;
      end Register_Untyped_Var;

      use Name_Id_Sets;
      use Ada.Containers;
   begin
      Trace (Me, "Compute the list of scenario variables");
      Unchecked_Free (Tree.Env.Scenario_Variables);
      Unchecked_Free (Tree.Env.Untyped_Variables);
      Var_Quantity := Count_Vars;

      Typed_List :=  new Scenario_Variable_Array (1 .. Var_Quantity);
      T_Curr := Typed_List'First;
      Untyped_List :=  new Untyped_Variable_Array (1 .. Var_Quantity);
      U_Curr := Untyped_List'First;

      For_Each_External_Variable_Declaration
        (Tree.Root, Recursive => Recursive,
         Callback => Register_Var'Unrestricted_Access);

      if Inconsistent_SC_Externals.Length = 0 then
         if T_Curr > Typed_List'Last then
            Tree.Env.Scenario_Variables := Typed_List;
         else
            Tree.Env.Scenario_Variables :=
              new Scenario_Variable_Array'(Typed_List (1 .. T_Curr - 1));
            Unchecked_Free (Typed_List);
         end if;
      else
         --  Moving SVs with inconsistent types to UVs
         T_Curr2 := T_Curr - 1 - Integer (Inconsistent_SC_Externals.Length);
         Tree.Env.Scenario_Variables :=
           new Scenario_Variable_Array (1 .. T_Curr2);
         T_Curr2 := 1;
         for I in 1 .. T_Curr - 1 loop
            if
              Inconsistent_SC_Externals.Contains (Typed_List (I).Ext_Name)
            then
               if
                 Not_Already
                   (Untyped_List, U_Curr, Typed_List (I).Ext_Name)
               then
                  Untyped_List (U_Curr) :=
                    (Name    => Typed_List (I).Ext_Name,
                     Default => Typed_List (I).Default,
                     Value   => Typed_List (I).Value);
                  U_Curr := U_Curr + 1;
               end if;
            else
               Tree.Env.Scenario_Variables (T_Curr2) := Typed_List (I);
               T_Curr2 := T_Curr2 + 1;
            end if;
         end loop;
         Unchecked_Free (Typed_List);
         Inconsistent_SC_Externals.Clear;
      end if;

      if U_Curr > Untyped_List'Last then
         Tree.Env.Untyped_Variables := Untyped_List;
      else
         Tree.Env.Untyped_Variables :=
           new Untyped_Variable_Array'(Untyped_List (1 .. U_Curr - 1));
         Unchecked_Free (Untyped_List);
      end if;

   end Compute_Scenario_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Self      : Project_Tree;
      Root_Only : Boolean := False)
      return Scenario_Variable_Array
   is
   begin
      return Scenario_Variables (Self.Data, Root_Only);
   end Scenario_Variables;

   -----------------------
   -- Untyped_Variables --
   -----------------------

   function Untyped_Variables
     (Self      : Project_Tree;
      Root_Only : Boolean := False) return Untyped_Variable_Array is
   begin
      return Untyped_Variables (Self.Data, Root_Only);
   end Untyped_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Tree      : Project_Tree_Data_Access;
      Root_Only : Boolean := False) return Scenario_Variable_Array
   is
      SVs : Scenario_Variable_Array_Access;
      UVs : Untyped_Variable_Array_Access;
   begin
      if Tree = null or else Tree.Is_Aggregated then
         return (1 .. 0 => <>);
      end if;

      if Root_Only then
         --  We need to save the actual values because otherwise
         --  Compute_Scenario_Variables will overwrite them.

         SVs := Tree.Env.Scenario_Variables;
         UVs := Tree.Env.Untyped_Variables;
         Tree.Env.Scenario_Variables := null;
         Tree.Env.Untyped_Variables  := null;

         Compute_Scenario_Variables (Tree, Recursive => False);

         declare
            Result : constant Scenario_Variable_Array :=
              Tree.Env.Scenario_Variables.all;
         begin
            Unchecked_Free (Tree.Env.Scenario_Variables);
            Unchecked_Free (Tree.Env.Untyped_Variables);
            Tree.Env.Scenario_Variables := SVs;
            Tree.Env.Untyped_Variables := UVs;
            return Result;
         end;
      end if;

      if Tree.Env.Scenario_Variables = null then
         Compute_Scenario_Variables (Tree);
      end if;

      for V of Tree.Env.Scenario_Variables.all loop
         V.Value :=
           GPR.Ext.Value_Of
             (Tree.Env.Env.External, V.Ext_Name, With_Default => V.Default);
      end loop;

      return Tree.Env.Scenario_Variables.all;
   end Scenario_Variables;

   -----------------------
   -- Untyped_Variables --
   -----------------------

   function Untyped_Variables
     (Tree      : Project_Tree_Data_Access;
      Root_Only : Boolean := False) return Untyped_Variable_Array
   is
      SVs : Scenario_Variable_Array_Access;
      UVs : Untyped_Variable_Array_Access;
   begin
      if Tree = null or else Tree.Is_Aggregated then
         return (1 .. 0 => <>);
      end if;

      if Root_Only then
         --  We need to save the actual values because otherwise
         --  Compute_Scenario_Variables will overwrite them.

         SVs := Tree.Env.Scenario_Variables;
         UVs := Tree.Env.Untyped_Variables;
         Tree.Env.Scenario_Variables := null;
         Tree.Env.Untyped_Variables  := null;

         Compute_Scenario_Variables (Tree, Recursive => False);

         declare
            Result : constant Untyped_Variable_Array :=
              Tree.Env.Untyped_Variables.all;
         begin
            Unchecked_Free (Tree.Env.Scenario_Variables);
            Unchecked_Free (Tree.Env.Untyped_Variables);
            Tree.Env.Scenario_Variables := SVs;
            Tree.Env.Untyped_Variables := UVs;
            return Result;
         end;
      end if;

      if Tree.Env.Untyped_Variables = null then
         Compute_Scenario_Variables (Tree);
      end if;

      for V of Tree.Env.Untyped_Variables.all loop
         V.Value :=
           GPR.Ext.Value_Of
             (Tree.Env.Env.External, V.Name, With_Default => V.Default);
      end loop;

      return Tree.Env.Untyped_Variables.all;
   end Untyped_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Self          : Project_Tree;
      External_Name : String;
      Root_Only     : Boolean := False) return Scenario_Variable
   is
      E : constant String := External_Name;
      Ext  : Name_Id;
      List : Scenario_Variable_Array_Access;
      Var  : Scenario_Variable;

      SVs : Scenario_Variable_Array_Access;
      UVs : Untyped_Variable_Array_Access;

      SV : Scenario_Variable;
   begin
      Ext := Get_String (E);

      if Root_Only then
         --  We need to save the actual values because otherwise
         --  Compute_Scenario_Variables will overwrite them.

         SVs := Self.Data.Env.Scenario_Variables;
         UVs := Self.Data.Env.Untyped_Variables;
         Self.Data.Env.Scenario_Variables := null;
         Self.Data.Env.Untyped_Variables  := null;

         Compute_Scenario_Variables (Self.Data, Recursive => False);

         for V of Self.Data.Env.Scenario_Variables.all loop
            if V.Ext_Name = Ext then
               SV := V;
               Unchecked_Free (Self.Data.Env.Scenario_Variables);
               Unchecked_Free (Self.Data.Env.Untyped_Variables);
               Self.Data.Env.Scenario_Variables := SVs;
               Self.Data.Env.Untyped_Variables := UVs;
               return SV;
            end if;
         end loop;

         Unchecked_Free (Self.Data.Env.Scenario_Variables);
         Unchecked_Free (Self.Data.Env.Untyped_Variables);
         Self.Data.Env.Scenario_Variables := SVs;
         Self.Data.Env.Untyped_Variables := UVs;
         return No_Variable;
      end if;

      if Self.Data.Env.Scenario_Variables = null then
         Compute_Scenario_Variables (Self.Data);
      end if;

      for V of Self.Data.Env.Scenario_Variables.all loop
         if V.Ext_Name = Ext then
            return V;
         end if;
      end loop;

      Var := Scenario_Variable'
        (Ext_Name    => Ext,
         Var_Name    => No_Name,
         Default     => No_Name,
         String_Type => Empty_Project_Node,  --   ???  Won't be able to edit it
         Tree_Ref    => null,
         Value       => No_Name,
         First_Project_Path => GPR.No_Path);

      List := Self.Data.Env.Scenario_Variables;
      Self.Data.Env.Scenario_Variables :=
        new Scenario_Variable_Array'
          (Self.Data.Env.Scenario_Variables.all & Var);
      Unchecked_Free (List);

      return Var;
   end Scenario_Variables;

   --------------------------
   -- Get_Untyped_Variable --
   --------------------------

   function Get_Untyped_Variable
     (Self          : Project_Tree;
      External_Name : String;
      Root_Only     : Boolean := False) return Untyped_Variable
   is
      Ext  : Name_Id;
      List : Untyped_Variable_Array_Access;
      Var  : Untyped_Variable;

      SVs : Scenario_Variable_Array_Access;
      UVs : Untyped_Variable_Array_Access;

      UV : Untyped_Variable;
   begin
      Ext := Get_String (External_Name);

      if Root_Only then
         --  We need to save the actual values because otherwise
         --  Compute_Scenario_Variables will overwrite them.

         SVs := Self.Data.Env.Scenario_Variables;
         UVs := Self.Data.Env.Untyped_Variables;
         Self.Data.Env.Scenario_Variables := null;
         Self.Data.Env.Untyped_Variables  := null;

         Compute_Scenario_Variables (Self.Data, Recursive => False);

         for V of Self.Data.Env.Untyped_Variables.all loop
            if V.Name = Ext then
               UV := V;
               Unchecked_Free (Self.Data.Env.Scenario_Variables);
               Unchecked_Free (Self.Data.Env.Untyped_Variables);
               Self.Data.Env.Scenario_Variables := SVs;
               Self.Data.Env.Untyped_Variables := UVs;
               return UV;
            end if;
         end loop;

         Unchecked_Free (Self.Data.Env.Scenario_Variables);
         Unchecked_Free (Self.Data.Env.Untyped_Variables);
         Self.Data.Env.Scenario_Variables := SVs;
         Self.Data.Env.Untyped_Variables := UVs;
         return No_Untyped_Variable;
      end if;

      if Self.Data.Env.Scenario_Variables = null then
         Compute_Scenario_Variables (Self.Data);
      end if;

      for V of Self.Data.Env.Untyped_Variables.all loop
         if V.Name = Ext then
            return V;
         end if;
      end loop;

      Var := Untyped_Variable'
        (Name        => Ext,
         Default     => No_Name,
         Value       => No_Name);

      List := Self.Data.Env.Untyped_Variables;
      Self.Data.Env.Untyped_Variables :=
        new Untyped_Variable_Array'
          (Self.Data.Env.Untyped_Variables.all & Var);
      Unchecked_Free (List);

      return Var;
   end Get_Untyped_Variable;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Ext_Name);
   end External_Name;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Var : Untyped_Variable) return String is
   begin
      return Get_String (Var.Name);
   end External_Name;

   ----------------------
   -- External_Default --
   ----------------------

   function External_Default (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Default);
   end External_Default;

   ----------------------
   -- External_Default --
   ----------------------

   function External_Default (Var : Untyped_Variable) return String is
   begin
      return Get_String (Var.Default);
   end External_Default;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Var   : in out Scenario_Variable;
      Value : String)
   is
   begin
      Var.Value := Get_String (Value);
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Var   : in out Untyped_Variable;
      Value : String)
   is
   begin
      Var.Value := Get_String (Value);
   end Set_Value;

   ------------------------
   -- Change_Environment --
   ------------------------

   procedure Change_Environment
     (Self   : Project_Tree;
      Vars   : Scenario_Variable_Array;
      UVars  : Untyped_Variable_Array := Empty_Untyped_Variable_Array) is
   begin
      for V in Vars'Range loop
         GPR.Ext.Add
           (Self.Data.Env.Env.External,
            Get_String (Vars (V).Ext_Name),
            Get_String (Vars (V).Value),
            GPR.Ext.From_Command_Line);
      end loop;
      for V in UVars'Range loop
         GPR.Ext.Add
           (Self.Data.Env.Env.External,
            Get_String (UVars (V).Name),
            Get_String (UVars (V).Value),
            GPR.Ext.From_Command_Line);
      end loop;
   end Change_Environment;

   ------------------------
   -- Change_Environment --
   ------------------------

   procedure Change_Environment
     (Self        : Project_Environment;
      Name, Value : String)
   is
   begin
      GPR.Ext.Add
        (Self.Env.External, Name, Value,
         GPR.Ext.From_Command_Line);
   end Change_Environment;

   -----------
   -- Value --
   -----------

   function Value (Self : Project_Environment; Name : String) return String is
      V : Name_Id;
   begin
      V := GPR.Ext.Value_Of (Self.Env.External, Get_String (Name));
      if V /= No_Name then
         return Get_String (V);
      else
         return "";
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Value);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Var : Untyped_Variable) return String is
   begin
      return Get_String (Var.Value);
   end Value;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Tree : GPR.Project_Tree_Ref;
      Path : Path_Name_Type) return GPR.Project_Id
   is
      Proj : Project_List := Tree.Projects;
   begin
      while Proj /= null loop
         if Proj.Project.Path.Display_Name = Path
           and then Proj.Project.Qualifier /= Configuration
         then
            return Proj.Project;
         end if;

         Proj := Proj.Next;
      end loop;

      return GPR.No_Project;
   end Get_View;

   --------------
   -- Get_View --
   --------------

   function Get_View (Project : Project_Type'Class) return GPR.Project_Id is
   begin
      if
        Project.Data = null
        or else Project.Data.Node = Empty_Project_Node
      then
         return GPR.No_Project;

      elsif Project.Data.View = GPR.No_Project then
         Project.Data.View :=
           Get_View
             (Project.Tree_View,
              GPR.Tree.Path_Name_Of (Project.Data.Node, Project.Tree_Tree));
      end if;

      return Project.Data.View;
   end Get_View;

   --------------------------------------------
   -- For_Each_External_Variable_Declaration --
   --------------------------------------------

   procedure For_Each_External_Variable_Declaration
     (Root_Project : Project_Type;
      Recursive    : Boolean;
      Callback     : External_Variable_Callback)
   is
      Iterator        : Project_Iterator := Start (Root_Project, Recursive);
      Current_Project : Project_Type;

      Tree : GPR.Project_Node_Tree_Ref;
      Var  : Project_Node_Id;
      Pkg  : Project_Node_Id;
      Prj  : Project_Node_Id;

   begin
      loop
         Current_Project := Current (Iterator);
         exit when Current_Project.Data = null;

         Tree := Current_Project.Data.Tree.Tree;
         Pkg  := Current_Project.Data.Node;
         Prj  := Current_Project.Data.Node;

         Current_Project.Data.Uses_Variables := False;

         --  For all the packages and the common section
         while Pkg /= Empty_Project_Node loop
            Var := First_Variable_Of (Pkg, Tree);

            while Var /= Empty_Project_Node loop

               if Kind_Of (Var, Tree) in
                   N_Typed_Variable_Declaration | N_Variable_Declaration
                 and then Is_External_Variable (Var, Tree)
               then
                  Callback (Var, Prj, Pkg, Current_Project);
               end if;

               if Kind_Of (Var, Tree) = N_Variable_Declaration
                 or else
                   (Kind_Of (Var, Tree) = N_Typed_Variable_Declaration
                    and then not Is_External_Variable (Var, Tree))
               then
                  if Active (Debug) then
                     Trace (Me, "Uses variable in " & Current_Project.Name);
                     Pretty_Print
                       (Var, Tree, Backward_Compatibility => False);
                  end if;
                  Current_Project.Data.Uses_Variables := True;
               end if;

               Var := Next_Variable (Var, Tree);
            end loop;

            if Pkg = Prj then
               Pkg := First_Package_Of (Prj, Tree);
            else
               Pkg := Next_Package_In_Project (Pkg, Tree);
            end if;
         end loop;

         Next (Iterator);
      end loop;
   end For_Each_External_Variable_Declaration;

   --------------
   -- Switches --
   --------------

   procedure Switches
     (Project          : Project_Type;
      In_Pkg           : String;
      File             : GNATCOLL.VFS.Virtual_File;
      Language         : String;
      Value            : out GNAT.Strings.String_List_Access;
      Is_Default_Value : out Boolean)
   is
      Val : Variable_Value;
   begin
      if Get_View (Project) /= GPR.No_Project then
         GPR.Util.Get_Switches
           (Source_File  => File_Name_Type
              (Get_String (File.Display_Base_Name)),
            Source_Lang  => Get_String (Language),
            Source_Prj   => Project.Data.View,
            Pkg_Name     => Get_String (To_Lower (In_Pkg)),
            Project_Tree => Project.Data.Tree.View,
            Value        => Val,
            Is_Default   => Is_Default_Value);

         Value := Variable_Value_To_List (Project, Val);
      else
         Value := null;
      end if;

      if Value = null then
         --  No switches
         Value := new String_List'(1 .. 0 => null);
      end if;
   end Switches;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Tree : GPR.Project_Node_Tree_Ref;
      Var  : Scenario_Variable) return String_List_Iterator
   is
      V, Expr : Project_Node_Id;
   begin
      case Kind_Of (Var.String_Type, Tree) is
         when N_String_Type_Declaration =>
            return (Current => First_Literal_String (Var.String_Type, Tree));

         when N_Attribute_Declaration
           |  N_Typed_Variable_Declaration
           |  N_Variable_Declaration =>

            V := Expression_Of (Var.String_Type, Tree);

            case Kind_Of (V, Tree) is
               when N_Expression =>
                  Expr := First_Term (V, Tree);
                  pragma Assert (Kind_Of (Expr, Tree) = N_Term);
                  Expr := Current_Term (Expr, Tree);

                  case Kind_Of (Expr, Tree) is
                     when N_Literal_String_List =>
                        return
                          (Current => First_Expression_In_List (Expr, Tree));

                     when N_External_Value =>
                        return
                          (Current => External_Default_Of (Expr, Tree));

                     when others =>
                        return (Current => V);
                  end case;

               when others =>
                  raise Program_Error;
            end case;

         when others =>
            raise Program_Error;
      end case;
   end Value_Of;

   ----------
   -- Done --
   ----------

   function Done (Iter : String_List_Iterator) return Boolean is
   begin
      return Iter.Current = Empty_Project_Node;
   end Done;

   ----------
   -- Next --
   ----------

   function Next
     (Tree : GPR.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator is
   begin
      pragma Assert (Iter.Current /= Empty_Project_Node);

      case Kind_Of (Iter.Current, Tree) is
         when N_Literal_String =>
            return (Current => Next_Literal_String (Iter.Current, Tree));

         when N_Expression =>
            return (Current => Next_Expression_In_List (Iter.Current, Tree));

         when others =>
            raise Program_Error;
      end case;
   end Next;

   ----------
   -- Data --
   ----------

   function Data
     (Tree : GPR.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return GPR.Name_Id is
   begin
      pragma Assert (Kind_Of (Iter.Current, Tree) = N_Literal_String);
      return String_Value_Of (Iter.Current, Tree);
   end Data;

   ------------------------
   -- Possible_Values_Of --
   ------------------------

   function Possible_Values_Of
     (Self : Project_Tree; Var : Scenario_Variable) return String_List
   is
      pragma Unreferenced (Self);
      Tree  : constant GPR.Project_Node_Tree_Ref := Var.Tree_Ref;
      Count : Natural := 0;
      Iter  : String_List_Iterator := Value_Of (Tree, Var);
   begin
      while not Done (Iter) loop
         Count := Count + 1;
         Iter := Next (Tree, Iter);
      end loop;

      declare
         Values : String_List (1 .. Count);
      begin
         Count := Values'First;

         Iter := Value_Of (Tree, Var);
         while not Done (Iter) loop
            Values (Count) := new String'
              (Get_Name_String (Data (Tree, Iter)));
            Count := Count + 1;
            Iter := Next (Tree, Iter);
         end loop;

         return Values;
      end;
   end Possible_Values_Of;

   ---------------------------
   -- Has_Imported_Projects --
   ---------------------------

   function Has_Imported_Projects (Project : Project_Type) return Boolean is
      Iter : constant Inner_Project_Iterator := Start
        (Project, Recursive => True, Direct_Only => True);
   begin
      return Current (Iter) /= No_Project;
   end Has_Imported_Projects;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Prj1, Prj2 : Project_Type) return Boolean is
   begin
      if Prj1.Data = null then
         return Prj2.Data = null;
      elsif Prj2.Data = null then
         return False;
      else
         return Prj1.Data.Node = Prj2.Data.Node
           and then Prj1.Data.Tree = Prj2.Data.Tree;
      end if;
   end "=";

   ---------
   -- "<" --
   ---------

   function Less (L, R : File_Info_Abstract'Class) return Boolean is
   begin
      return L < R;
   end Less;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : File_Info) return Boolean is
   begin
      return L.Project.Project_Path < R.Project.Project_Path;
   end "<";

   ----------------------
   -- Extended_Project --
   ----------------------

   function Extended_Project
     (Project : Project_Type) return Project_Type
   is
      Tree     : constant GPR.Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      Extended : constant Project_Node_Id := Extended_Project_Of
        (Project_Declaration_Of (Project.Data.Node, Tree), Tree);

   begin
      if Extended = Empty_Project_Node then
         return No_Project;
      else
         return Project_Type
           (Project_From_Name
              (Project.Data.Tree, GPR.Tree.Name_Of (Extended, Tree)));
      end if;
   end Extended_Project;

   ------------------------------------
   -- Extended_Projects_Source_Files --
   ------------------------------------

   function Extended_Projects_Source_Files
     (Project : Project_Type) return GNATCOLL.VFS.File_Array_Access
   is
      P : Project_Type := Project;

      Result, Files : GNATCOLL.VFS.File_Array_Access;
   begin
      if Project.Data = null
        or else Project.Data.Files = null
      then
         return new File_Array (1 .. 0);
      end if;

      while P /= No_Project loop
         Files := P.Source_Files (Recursive => False);
         Append (Result, Files.all);
         Unchecked_Free (Files);

         P := Extended_Project (P);
      end loop;

      return Result;
   end Extended_Projects_Source_Files;

   -----------------------
   -- Extending_Project --
   -----------------------

   function Extending_Project
     (Project : Project_Type; Recurse : Boolean := False) return Project_Type
   is
      Tree      : constant GPR.Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      Extending : Project_Node_Id := Empty_Project_Node;
      Extended  : Project_Node_Id := Project.Data.Node;
   begin
      while Project_Declaration_Of (Extended, Tree) /= Empty_Project_Node loop
         Extending := Extending_Project_Of
           (Project_Declaration_Of (Extended, Tree), Tree);

         exit when not Recurse;

         --  Case of following extension chain: if we reached the of the chain,
         --  go back one step (to the last non-empty node) and exit.

         if Extending = Empty_Project_Node then
            Extending := Extended;
            exit;
         end if;

         --  Iterate

         Extended := Extending;
      end loop;

      if Extending = Empty_Project_Node then
         return No_Project;
      else
         return Project_Type
           (Project_From_Path
              (Project.Data.Tree,
               GPR.Tree.Path_Name_Of (Extending, Tree)));
      end if;
   end Extending_Project;

   ----------------------
   -- Externally_Built --
   ----------------------

   function Externally_Built (Project : Project_Type) return Boolean is
   begin
      return Get_View (Project).Externally_Built;
   end Externally_Built;

   -----------
   -- Build --
   -----------

   function Build
     (Package_Name, Attribute_Name : String) return Attribute_Pkg_String is
   begin
      return Attribute_Pkg_String
        (To_Lower (Package_Name) & '#' & To_Lower (Attribute_Name));
   end Build;

   function Build
     (Package_Name, Attribute_Name : String) return Attribute_Pkg_List is
   begin
      return Attribute_Pkg_List
        (To_Lower (Package_Name) & '#' & To_Lower (Attribute_Name));
   end Build;

   ------------------------
   -- Delete_File_Suffix --
   ------------------------

   function Delete_File_Suffix
     (Filename : Filesystem_String; Project : Project_Type) return Natural
   is
      View   : constant Project_Id := Get_View (Project);
      Lang   : Language_Ptr;
      Suffix : Name_Id;
   begin
      --  View will be null when called from the project wizard

      if View /= GPR.No_Project then
         Lang := View.Languages;
         while Lang /= null loop
            Suffix := Name_Id (Lang.Config.Naming_Data.Spec_Suffix);
            if Suffix /= No_Name
              and then Utils.Ends_With (+Filename, Get_Name_String (Suffix))
            then
               return Filename'Last - Natural (Length_Of_Name (Suffix));
            end if;

            Suffix := Name_Id (Lang.Config.Naming_Data.Body_Suffix);
            if Suffix /= No_Name
              and then Utils.Ends_With (+Filename, Get_Name_String (Suffix))
            then
               return Filename'Last - Natural (Length_Of_Name (Suffix));
            end if;

            Lang := Lang.Next;
         end loop;
      end if;

      --  Check the default naming scheme as well ? Otherwise, it might happen
      --  that a project has its own naming scheme, but still references files
      --  in the runtime with the default naming scheme.

      declare
         Ext : constant String :=
                 GNAT.Directory_Operations.File_Extension (+Filename);
      begin
         if  Ext = ".ads" or else Ext = ".adb" then
            return Filename'Last - 4;
         end if;
      end;

      return Filename'Last;
   end Delete_File_Suffix;

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name
     (Project        : Project_Type;
      File           : GNATCOLL.VFS.Filesystem_String;
      Include_Suffix : Boolean := False) return Filesystem_String
   is
      Base        : constant Filesystem_String := Base_Name (File);
      Exec_Name   : File_Name_Type;
      Main_Source : Source_Id;

   begin
      if Project = No_Project then
         Trace (Me, "Executable_Name: no project");
         --  Simply remove the current extension, since we don't have any
         --  information on the file itself.
         return Base
           (Base'First .. Delete_File_Suffix (Base, Project));

      else
         declare
            Norm : String := +Base;
         begin
            Osint.Canonical_Case_File_Name (Norm);
            Main_Source := Find_Source
              (In_Tree   => Project.Data.Tree.View,
               Project   => Project.Data.View,
               Base_Name => File_Name_Type (Get_String (Norm)));
         end;

         if Main_Source = No_Source then
            Trace (Me, "Executable_Name: source not found ("
                   & (+Base) & ')');
            return Base
              (Base'First .. Delete_File_Suffix (Base, Project));
         end if;

         --  Do not include the suffix: it might be incorrect if we user will
         --  actually use a cross-compiler, since the suffix's default value
         --  depends on the host.

         Exec_Name := Executable_Of
           (Project  => Project.Data.View,
            Shared   => Project.Data.Tree.View.Shared,
            Main     => Main_Source.File,
            Index    => Main_Source.Index,
            Language => Get_Name_String (Main_Source.Language.Name),
            Include_Suffix => Include_Suffix);
         return +(Get_String (Exec_Name));
      end if;
   end Executable_Name;

   ------------------
   -- Create_Flags --
   ------------------

   function Create_Flags
     (On_Error        : GPR.Error_Handler;
      Require_Sources : Boolean := True;
      Ignore_Missing_With : Boolean := False;
      Report_Missing_Dirs : Boolean := True) return Processing_Flags is
   begin
      if Require_Sources then
         return Create_Flags
           (Report_Error               => On_Error,
            When_No_Sources            => Warning,
            Require_Sources_Other_Lang => True,
            Compiler_Driver_Mandatory  => False,
            Allow_Duplicate_Basenames  => True,
            Require_Obj_Dirs           =>
               (if Report_Missing_Dirs then Warning else Silent),
            Allow_Invalid_External     => Warning,
            Missing_Source_Files       => Warning,
            Ignore_Missing_With        => Ignore_Missing_With);
      else
         return Create_Flags
           (Report_Error               => On_Error,
            When_No_Sources            => Silent,
            Require_Sources_Other_Lang => False,
            Compiler_Driver_Mandatory  => False,
            Allow_Duplicate_Basenames  => True,
            Require_Obj_Dirs           =>
               (if Report_Missing_Dirs then Warning else Silent),
            Allow_Invalid_External     => Silent,
            Missing_Source_Files       => Warning,
            Ignore_Missing_With        => Ignore_Missing_With);
      end if;
   end Create_Flags;

   ----------------------------
   -- Has_Multi_Unit_Sources --
   ----------------------------

   function Has_Multi_Unit_Sources (Project : Project_Type) return Boolean is
      View : constant Project_Id := Get_View (Project);
   begin
      if View /= GPR.No_Project then
         return View.Has_Multi_Unit_Sources;
      end if;
      return False;
   end Has_Multi_Unit_Sources;

   -----------------------
   -- Project_From_Name --
   -----------------------

   function Project_From_Name
     (Tree : Project_Tree_Data_Access;
      Name : GPR.Name_Id) return Project_Type'Class
   is
      Tree_For_Map : Project_Tree_Data_Access;

      P_Cursor, P_Found : Project_Htables.Cursor;
      Name_Found : Boolean := False;

      --  Name is a base name (for now), but the htable is indexed on the
      --  full path of the project. So we need to traverse all its elements.
      --  In the case of aggregate projects, we return No_Project if multiple
      --  projects match.

      Normalized : constant Filesystem_String :=
         Create (+Get_String (Name)).Base_Name
            (Suffix => +GPR.Project_File_Extension, Normalize => True);

      --  The name of a project is not related to file names, and is always
      --  case-insensitive. So we convert to lower-case here. However, if we
      --  want a version of Project_From_Name that takes a path, we will need
      --  to use the filesystem's casing.
      --
      --  We can't compare project names and file names, because child projects
      --  have names like "p.main" when the file name is "p-main".

      N : constant String := To_Lower (+Normalized);

   begin
      if Tree = null or else Tree.Tree = null then
         Trace (Me, "Project_From_Name: Registry not initialized");
         return No_Project;

      else
         Tree_For_Map := Tree.Root.Data.Tree_For_Map;
         P_Cursor := Tree_For_Map.Projects.First;

         if Project_Qualifier_Of (Tree.Root.Data.Node, Tree.Tree) =
           GPR.Aggregate
         then
            while P_Cursor /= Project_Htables.No_Element loop
               if To_Lower (Element (P_Cursor).Name) = N then
                  if Name_Found then
                     Trace (Me, "Multiple projects with same name ("
                            & N & ')');
                     return No_Project;
                  else
                     Name_Found := True;
                     P_Found := P_Cursor;
                  end if;
               end if;

               Next (P_Cursor);
            end loop;

            if Name_Found then
               return Element (P_Found);
            end if;

         else
            while P_Cursor /= Project_Htables.No_Element loop
               if To_Lower (Element (P_Cursor).Name) = N then
                  return Element (P_Cursor);
               end if;
               Next (P_Cursor);
            end loop;
         end if;

         Trace (Me, "Get_Project_From_Name: "
                & Get_String (Name) & " wasn't found");
         return No_Project;
      end if;
   end Project_From_Name;

   -----------------------
   -- Project_From_Name --
   -----------------------

   function Project_From_Name
     (Self : Project_Tree'Class; Name : String) return Project_Type is
   begin
      return Project_Type (Project_From_Name (Self.Data, Get_String (Name)));
   end Project_From_Name;

   -----------------------
   -- Project_From_Path --
   -----------------------

   function Project_From_Path
     (Self : Project_Tree'Class;
      Path : Virtual_File) return Project_Type
   is
      Tree_For_Map : constant Project_Tree_Data_Access :=
         Self.Data.Root.Data.Tree_For_Map;
      --  An access to the root tree

      VF : constant GNATCOLL.VFS.Virtual_File :=
        Create (Normalize_Pathname (Path.Full_Name, Resolve_Links => False));
      P_Cursor : constant Project_Htables.Cursor :=
        Tree_For_Map.Projects.Find (VF);
   begin
      if not Has_Element (P_Cursor) then
         return No_Project;
      end if;
      return Element (P_Cursor);
   end Project_From_Path;

   -----------------------
   -- Project_From_Path --
   -----------------------

   function Project_From_Path
     (Tree    : Project_Tree_Data_Access;
      Path_Id : Path_Name_Type) return Project_Type'Class
   is
      Tree_For_Map : constant Project_Tree_Data_Access :=
         Tree.Root.Data.Tree_For_Map;
      --  An access to the root tree

      P_Cursor : constant Project_Htables.Cursor :=
        Tree_For_Map.Projects.Find (Create (+Get_String (Path_Id)));
   begin
      if not Has_Element (P_Cursor) then
         return No_Project;
      end if;
      return Element (P_Cursor);
   end Project_From_Path;

   ----------------------
   -- Set_Trusted_Mode --
   ----------------------

   procedure Set_Trusted_Mode
     (Self : in out Project_Environment; Trusted : Boolean := True) is
   begin
      Self.Trusted_Mode := Trusted;
      Opt.Follow_Links_For_Files := not Trusted;
      Opt.Follow_Links_For_Dirs  := not Trusted;
      GNATCOLL.VFS.Symbolic_Links_Support (Active => not Trusted);
   end Set_Trusted_Mode;

   ------------------
   -- Trusted_Mode --
   ------------------

   function Trusted_Mode (Self : Project_Environment) return Boolean is
   begin
      return Self.Trusted_Mode;
   end Trusted_Mode;

   --------------------------------
   -- Set_Predefined_Source_Path --
   --------------------------------

   procedure Set_Predefined_Source_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array) is
   begin
      Unchecked_Free (Self.Predefined_Source_Files);
      Unchecked_Free (Self.Predefined_Source_Path);
      Self.Predefined_Source_Path := new File_Array'(Path);
   end Set_Predefined_Source_Path;

   procedure Set_Predefined_Object_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array) is
   begin
      Unchecked_Free (Self.Predefined_Object_Path);
      Self.Predefined_Object_Path := new File_Array'(Path);
   end Set_Predefined_Object_Path;

   procedure Set_Predefined_Project_Path
     (Self : in out Project_Environment; Path : GNATCOLL.VFS.File_Array) is
   begin
      Unchecked_Free (Self.Predefined_Project_Path);
      Self.Predefined_Project_Path := new File_Array'(Path);
   end Set_Predefined_Project_Path;

   ----------------------------
   -- Predefined_Source_Path --
   ----------------------------

   function Predefined_Source_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array is
   begin
      if Self.Predefined_Source_Path = null then
         return (1 .. 0 => GNATCOLL.VFS.No_File);
      else
         return Self.Predefined_Source_Path.all;
      end if;
   end Predefined_Source_Path;

   function Predefined_Object_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array is
   begin
      if Self.Predefined_Object_Path = null then
         return (1 .. 0 => GNATCOLL.VFS.No_File);
      else
         return Self.Predefined_Object_Path.all;
      end if;
   end Predefined_Object_Path;

   function Predefined_Project_Path
     (Self : Project_Environment) return GNATCOLL.VFS.File_Array
   is
      Current : Virtual_File;
   begin
      if Self.Predefined_Project_Path = null then
         Current := Create (Get_Current_Dir);
         return (1 .. 1 => Current);
      else
         return Self.Predefined_Project_Path.all;
      end if;
   end Predefined_Project_Path;

   ------------------------
   -- Set_Build_Tree_Dir --
   ------------------------

   procedure Set_Build_Tree_Dir
     (Self : in out Project_Environment;
      Dir  : GNATCOLL.VFS.Filesystem_String)
   is
      pragma Unreferenced (Self);
   begin
      Free (GPR.Build_Tree_Dir);
      if Dir = "" then
         GPR.Build_Tree_Dir := null;
      else
         GPR.Build_Tree_Dir := new String'(+Dir);
      end if;
   end Set_Build_Tree_Dir;

   --------------------
   -- Build_Tree_Dir --
   --------------------

   function Build_Tree_Dir
     (Self : Project_Environment) return GNATCOLL.VFS.Filesystem_String
   is
      pragma Unreferenced (Self);
   begin
      if GPR.Build_Tree_Dir = null then
         return "";
      else
         return +GPR.Build_Tree_Dir.all;
      end if;
   end Build_Tree_Dir;

   ------------------
   -- Set_Root_Dir --
   ------------------

   procedure Set_Root_Dir
     (Self : in out Project_Environment;
      Dir  : GNATCOLL.VFS.Filesystem_String)
   is
      pragma Unreferenced (Self);
   begin
      Free (GPR.Root_Dir);
      if Dir = "" then
         GPR.Root_Dir := null;
      else
         GPR.Root_Dir := new String'(+Dir);
      end if;
   end Set_Root_Dir;

   --------------
   -- Root_Dir --
   --------------

   function Root_Dir
     (Self : Project_Environment) return GNATCOLL.VFS.Filesystem_String
   is
      pragma Unreferenced (Self);
   begin
      if GPR.Root_Dir = null then
         return "";
      else
         return +GPR.Root_Dir.all;
      end if;
   end Root_Dir;

   -----------------------
   -- Set_Object_Subdir --
   -----------------------

   procedure Set_Object_Subdir
     (Self   : in out Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String)
   is
      pragma Unreferenced (Self);
   begin
      Free (GPR.Subdirs);
      if Subdir = "." then
         GPR.Subdirs := null;
      else
         GPR.Subdirs := new String'(+Subdir);
      end if;
   end Set_Object_Subdir;

   -------------------
   -- Object_Subdir --
   -------------------

   function Object_Subdir
     (Self   : Project_Environment) return GNATCOLL.VFS.Filesystem_String
   is
      pragma Unreferenced (Self);
   begin
      if GPR.Subdirs = null then
         return "";
      else
         return +GPR.Subdirs.all;
      end if;
   end Object_Subdir;

   ----------------------
   -- Set_Xrefs_Subdir --
   ----------------------

   procedure Set_Xrefs_Subdir
     (Self   : in out Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String) is
   begin
      Free (Self.Xrefs_Subdir);
      Self.Xrefs_Subdir := new String'(+Subdir);
   end Set_Xrefs_Subdir;

   function Xrefs_Subdir
     (Self   : Project_Environment) return GNATCOLL.VFS.Filesystem_String is
   begin
      if Self.Xrefs_Subdir = null then
         return "";
      else
         return +Self.Xrefs_Subdir.all;
      end if;
   end Xrefs_Subdir;

   -----------------------------
   -- Predefined_Source_Files --
   -----------------------------

   function Predefined_Source_Files
     (Self : access Project_Environment) return GNATCOLL.VFS.File_Array is
   begin
      --  ??? A nicer way would be to implement this with a predefined project,
      --  and rely on the project parser to return the source
      --  files. Unfortunately, this doesn't work with the current
      --  implementation of this parser, since one cannot have two separate
      --  project hierarchies at the same time.

      if Self.Predefined_Source_Files = null
        and then Self.Predefined_Source_Path /= null
      then
         Self.Predefined_Source_Files := Read_Files_From_Dirs
           (Self.Predefined_Source_Path.all);
      end if;

      if Self.Predefined_Source_Files = null then
         return Empty_File_Array;
      else
         return Self.Predefined_Source_Files.all;
      end if;
   end Predefined_Source_Files;

   ------------------
   -- Data_Factory --
   ------------------

   function Data_Factory (Self : Project_Tree) return Project_Data_Access is
      pragma Unreferenced (Self);
   begin
      return new Project_Data;
   end Data_Factory;

   ----------
   -- Data --
   ----------

   function Data (Project : Project_Type) return Project_Data_Access is
   begin
      return Project.Data;
   end Data;

   -------------
   -- On_Free --
   -------------

   procedure On_Free (Self : in out Project_Data) is
   begin
      Unchecked_Free (Self.Imported_Projects.Items);
      Unchecked_Free (Self.Importing_Projects);
      Reset_View (Self);
   end On_Free;

   ----------------
   -- Reset_View --
   ----------------

   procedure Reset_View (Self : in out Project_Data'Class) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Basename_To_Info_Cache.Map,
         Basename_To_Info_Cache_Map_Access);
   begin
      Self.View := GPR.No_Project;
      --  No need to reset Self.Imported_Projects, since this doesn't
      --  change when the view changes.

      Unchecked_Free (Self.Non_Recursive_Include_Path);
      Unchecked_Free (Self.Files);

      if Self.Base_Name_To_Full_Path /= null then
         Self.Base_Name_To_Full_Path.Clear;
         Unchecked_Free (Self.Base_Name_To_Full_Path);
         Self.Base_Name_To_Full_Path := null;
      end if;

      Self.View_Is_Complete := True;
   end Reset_View;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Project_Type) is
   begin
      if Self.Data /= null then
         Self.Data.Refcount := Self.Data.Refcount + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Project_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Data'Class, Project_Data_Access);
      Data : Project_Data_Access := Self.Data;
   begin
      --  Make Finalize idempotent, since it could be called several times.
      --  See RM 7.6.1 (24)

      Self.Data := null;

      --  We never finalize unless Tree is null: the tree is set to null when
      --  the project_tree is unloaded. That means user cares about memory
      --  management. If we try to finalize when unload hasn't been called, and
      --  because the tree owns references to the project, this means Finalize
      --  is called by GNAT as part of processing the finalization_lists. In
      --  that case, it seems we always end up in a case where we access
      --  already deallocated memory.

      if Data /= null then
         Data.Refcount := Data.Refcount - 1;
         if Data.Refcount = 0
           and then Data.Tree = null
         then
            On_Free (Data.all);
            Unchecked_Free (Data);
            Data := null;
         end if;
      end if;
   end Finalize;

   ----------------------------
   -- Add_Language_Extension --
   ----------------------------

   procedure Add_Language_Extension
     (Self          : in out Project_Environment;
      Language_Name : String;
      Extension     : String)
   is
      Ext  : String := Extension;
   begin
      Osint.Canonical_Case_File_Name (Ext);
      Self.Extensions.Include (Ext, Get_String (To_Lower (Language_Name)));
   end Add_Language_Extension;

   -----------------------------------------
   -- Register_Default_Language_Extension --
   -----------------------------------------

   procedure Register_Default_Language_Extension
     (Self                : in out Project_Environment;
      Language_Name       : String;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String;
      Obj_Suffix          : String := ".o")
   is
      Spec, Impl, Obj : String_Access;
      Spec_Suff  : String := Default_Spec_Suffix;
      Impl_Suff  : String := Default_Body_Suffix;
   begin
      --  GNAT doesn't allow empty suffixes, and will display an error when
      --  the view is recomputed, in that case. Therefore we substitute dummy
      --  empty suffixes instead

      if Default_Spec_Suffix = "" then
         Spec := new String'(Dummy_Suffix);
      else
         Osint.Canonical_Case_File_Name (Spec_Suff);
         Spec := new String'(Spec_Suff);
      end if;

      if Default_Body_Suffix = "" then
         Impl := new String'(Dummy_Suffix);
      else
         Osint.Canonical_Case_File_Name (Impl_Suff);
         Impl := new String'(Impl_Suff);
      end if;

      if Obj_Suffix = "" then
         Obj := new String'("-");
      else
         Obj := new String'(Obj_Suffix);
      end if;

      Self.Naming_Schemes := new Naming_Scheme_Record'
        (Language            => new String'(To_Lower (Language_Name)),
         Default_Spec_Suffix => Spec,
         Default_Body_Suffix => Impl,
         Obj_Suffix          => Obj,
         Next                => Self.Naming_Schemes);
   end Register_Default_Language_Extension;

   -------------------------
   -- Default_Spec_Suffix --
   -------------------------

   function Default_Spec_Suffix
     (Self          : Project_Environment'Class;
      Language_Name : String) return String
   is
      Tmp  : Naming_Scheme_Access := Self.Naming_Schemes;
      Lang : constant String := To_Lower (Language_Name);
   begin
      while Tmp /= null loop
         if Tmp.Language.all = Lang then
            return Tmp.Default_Spec_Suffix.all;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return "";
   end Default_Spec_Suffix;

   -------------------------
   -- Default_Body_Suffix --
   -------------------------

   function Default_Body_Suffix
     (Self          : Project_Environment'Class;
      Language_Name : String) return String
   is
      Tmp  : Naming_Scheme_Access := Self.Naming_Schemes;
      Lang : constant String := To_Lower (Language_Name);
   begin
      while Tmp /= null loop
         if Tmp.Language.all = Lang then
            return Tmp.Default_Body_Suffix.all;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return "";
   end Default_Body_Suffix;

   ---------------------------
   -- Registered_Extensions --
   ---------------------------

   function Registered_Extensions
     (Self          : Project_Environment;
      Language_Name : String) return GNAT.Strings.String_List
   is
      Lang    : constant String := To_Lower (Language_Name);
      Lang_Id : constant Name_Id := Get_String (Lang);
      Iter    : Extensions_Languages.Cursor := Self.Extensions.First;
      Count   : Natural := 0;
   begin
      while Has_Element (Iter) loop
         if Element (Iter) = Lang_Id then
            Count := Count + 1;
         end if;

         Next (Iter);
      end loop;

      declare
         Args : String_List (1 .. Count);
      begin
         Count := Args'First;
         Iter := Self.Extensions.First;
         while Has_Element (Iter) loop
            if Element (Iter) = Lang_Id then
               Args (Count) := new String'(Key (Iter));
               Count := Count + 1;
            end if;

            Next (Iter);
         end loop;

         return Args;
      end;
   end Registered_Extensions;

   ------------------
   -- Root_Project --
   ------------------

   function Root_Project (Self : Project_Tree'Class) return Project_Type is
   begin
      if Self.Data = null then
         return No_Project;
      else
         return Self.Data.Root;
      end if;
   end Root_Project;

   ----------------------------------
   -- Directory_Belongs_To_Project --
   ----------------------------------

   function Directory_Belongs_To_Project
     (Self        : Project_Tree;
      Directory   : GNATCOLL.VFS.Filesystem_String;
      Direct_Only : Boolean := True) return Boolean
   is
      Curs : constant Directory_Statuses.Cursor :=
        Self.Data.Directories.Find (Name_As_Directory (Directory));
      Belong : Directory_Dependency;
   begin
      if Has_Element (Curs) then
         Belong := Element (Curs);
         return Belong = Direct
           or else (not Direct_Only and then Belong = As_Parent);
      else
         return False;
      end if;
   end Directory_Belongs_To_Project;

   ----------
   -- Hash --
   ----------

   function Hash
     (File : GNATCOLL.VFS.Filesystem_String) return Ada.Containers.Hash_Type is
   begin
      if GNATCOLL.VFS_Utils.Local_Host_Is_Case_Sensitive then
         return Ada.Strings.Hash (+File);
      else
         return Ada.Strings.Hash_Case_Insensitive (+File);
      end if;
   end Hash;

   function Hash (Node : Project_Node_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (GPR.Tree.Hash (Node));
   end Hash;

   ------------------
   -- Include_File --
   ------------------

   procedure Include_File
     (Map  : in out Names_Files.Map;
      Key  : GNATCOLL.VFS.Filesystem_String;
      Elem : Source_File_Data)
   is
      M_Cur       : Names_Files.Cursor;
      Inserted    : Boolean;
      Elem_Access : Source_File_Data_Access;

   begin
      Map.Insert (Key, Elem, M_Cur, Inserted);
      if Inserted then
         return;
      end if;

      declare
         Found_Elem : constant Names_Files.Reference_Type :=
                        Map.Reference (M_Cur);
      begin
         if Found_Elem.Project = Elem.Project
           and then Found_Elem.File = Elem.File
         then
            --  Exactly same file, nothing has to be done.
            return;

         elsif Found_Elem.Next = null then
            Found_Elem.Next := new Source_File_Data'(Elem);

         else
            --  Look through other files with same base name and add elem
            --  if not present.

            Elem_Access := Found_Elem.Next;
            loop
               if Elem_Access.Project = Elem.Project
                 and then Elem_Access.File = Elem.File
               then
                  return;
               end if;

               if Elem_Access.Next = null then
                  Elem_Access.Next := new Source_File_Data'(Elem);
                  return;
               end if;

               Elem_Access := Elem_Access.Next;
            end loop;
         end if;
      end;
   end Include_File;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : GNATCOLL.VFS.Filesystem_String) return Boolean is
   begin
      --  ??? In GPS, we used to take into account the sensitive of the build
      --  host. However, this wasn't correct either, because it was computed
      --  at elaboration time, so always with local_host. Ideally, we should
      --  have access to a Project_Environment to find this out.
      return Equal
        (+F1, +F2,
         Case_Sensitive => GNATCOLL.VFS_Utils.Local_Host_Is_Case_Sensitive);
   end Equal;

   ----------------------
   -- Reload_If_Needed --
   ----------------------

   procedure Reload_If_Needed
     (Self     : in out Project_Tree;
      Reloaded : out Boolean;
      Recompute_View : Boolean := False;
      Errors   : Error_Report := null)
   is
      Iter : Inner_Project_Iterator;
   begin
      Iter     := Start (Self.Root_Project);
      Reloaded := False;

      while Current (Iter) /= No_Project loop
         if File_Time_Stamp (Project_Path (Current (Iter))) >
           Self.Data.Timestamp
         then
            Trace (Me, "Reload_If_Needed: timestamp has changed for "
                   & Current (Iter).Name);
            Reloaded := True;
            exit;
         end if;

         Next (Iter);
      end loop;

      if Reloaded then
         Self.Load
           (Env                => Self.Data.Env,
            Root_Project_Path  => Project_Path (Self.Root_Project),
            Recompute_View     => Recompute_View,
            Errors             => Errors);
      else
         Trace (Me, "Reload_If_Needed: nothing to do, timestamp unchanged");
      end if;
   end Reload_If_Needed;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self               : in out Project_Tree;
      Root_Project_Path  : GNATCOLL.VFS.Virtual_File;
      Env                : Project_Environment_Access := null;
      Packages_To_Check  : GNAT.Strings.String_List_Access := No_Packs;
      Errors             : Error_Report := null;
      Recompute_View     : Boolean := True;
      Report_Missing_Dirs : Boolean := True)
   is
      Block_Me : constant Block_Trace_Handle :=
         Create (Me, Root_Project_Path.Display_Full_Name);
      Tmp : Project_Tree'Class := Self;  --  Must use same tag
      Previous_Project : Virtual_File;
      Previous_Status  : Project_Status;
      Success          : Boolean;
      Project          : Project_Node_Id;
      Project_File     : GNATCOLL.VFS.Virtual_File := Root_Project_Path;
      Pth              : Path_Name_Type;
   begin
      Sinput.Clear_Source_File_Table;
      Sinput.Reset_First;

      if Active (Me_Gnat) then
         GPR.Current_Verbosity := GPR.High;
      end if;

      Set_Host_Targets_List;

      if Self.Data /= null and then Self.Data.Root /= No_Project then
         Previous_Project := Self.Root_Project.Project_Path;
         Previous_Status  := Self.Data.Status;

      else
         Previous_Project := GNATCOLL.VFS.No_File;
         Previous_Status  := Default;
      end if;

      if Env /= null and then Env.Config_File.Is_Regular_File then
         Env.Set_Target_And_Runtime_From_Config;
      end if;

      --  Looking for the project file in predefined paths if the default
      --  project path has been initialized.
      if Env /= null and then Is_Initialized (Env.Env.Project_Path) then
         Find_Project
           (Env.Env.Project_Path,
            Root_Project_Path.Display_Full_Name,
            "",
            Pth);

         if Pth /= No_Path then
            Project_File := Create (+Get_Name_String (Pth));
         end if;
      end if;

      if not Is_Regular_File (Project_File) then
         Trace (Me, "Load: " & Display_Full_Name (Root_Project_Path)
                & " is not a regular file");
         Project_File :=
            Create
              (Normalize_Pathname
                (Full_Name (Project_File) & Project_File_Extension,
                 Resolve_Links => False));
         if not Is_Regular_File (Project_File) then
            Trace
              (Me, "Load: " & Display_Full_Name (Project_File)
               & " is not a regular file");

            if Errors /= null then
               Errors (Display_Full_Name (Root_Project_Path)
                       & " is not a regular file");
            end if;

            raise Invalid_Project;
         end if;
      end if;

      Tmp.Data := new Project_Tree_Data (Is_Aggregated => False);

      if Env = null then
         if Self.Data = null or else Self.Data.Env = null then
            Initialize (Tmp.Data.Env);
         else
            Tmp.Data.Env := Self.Data.Env;
         end if;
      else
         Tmp.Data.Env := Env;
      end if;

      --  Force a recomputation of the timestamp the next time Recompute_View
      --  is called.
      Tmp.Data.Timestamp := GNATCOLL.Utils.No_Time;

      Register_Specific_Attributes;

      Trace (Me, "Initial parsing to check the syntax");
      Internal_Load
        (Tmp, Project_File, Errors,
         Report_Syntax_Errors => True,
         Project              => Project,
         Packages_To_Check    => Packages_To_Check,
         Recompute_View       => False,
         Report_Missing_Dirs  => Report_Missing_Dirs,
         Implicit_Project     => False);

      GPR.Err.Initialize;  --  Clear errors

      if Project = Empty_Project_Node then
         --  Reset the list of error messages, and keep current project
         --  unchanged

         if Self.Data = null then
            Self.Load_Empty_Project (Env => Tmp.Data.Env);
         end if;

         Free (Tmp.Data.View);
         Free (Tmp.Data);

         Trace (Me, "empty_node after parsing the tree");
         raise Invalid_Project;
      end if;

      --  We know the project is syntactically correct, so we can go on with
      --  the processing (we can't reuse the previous parsing, because we need
      --  to Unload first.

      if Self.Data = null then
         Self.Data := Tmp.Data;
      else
         Project_Tree'Class (Self).Unload;
         Self.Data.Timestamp := GNATCOLL.Utils.No_Time;
         Self.Data.Env := Tmp.Data.Env;

         Free (Tmp.Data.View);
         Free (Tmp.Data);
      end if;

      Trace (Me, "Parsing again, now that we know the syntax is correct");
      Internal_Load
        (Self, Project_File, Errors,
         Report_Syntax_Errors => False,   --  already done above
         Project              => Project,
         Packages_To_Check    => Packages_To_Check,
         Recompute_View       => Recompute_View,
         Report_Missing_Dirs  => Report_Missing_Dirs,
         Implicit_Project     => False);

      if Previous_Status = Default then
         Trace (Me, "Remove previous default project on disk, no longer used");
         Delete (Previous_Project, Success);
      end if;
   end Load;

   ---------------------
   -- Set_Config_File --
   ---------------------

   procedure Set_Config_File
     (Self        : in out Project_Environment;
      Config_File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.Config_File := Config_File;
   end Set_Config_File;

   -------------------------------
   -- Set_Automatic_Config_File --
   -------------------------------

   procedure Set_Automatic_Config_File
     (Self        : in out Project_Environment;
      Autoconf    : Boolean := True) is
   begin
      Self.Autoconf := Autoconf;
   end Set_Automatic_Config_File;

   --------------------
   -- Add_Config_Dir --
   --------------------

   procedure Add_Config_Dir
     (Self      : in out Project_Environment;
      Directory : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self);
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Directory.Display_Full_Name);
      GPR.Conf.Add_Db_Switch_Arg (Name_Find);
   end Add_Config_Dir;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Project_Environment_Access;
      IDE_Mode : Boolean := False) is
   begin
      if Self = null then
         Self := new Project_Environment;
      end if;

      GPR.Tree.Initialize (Self.Env, Create_Flags (null));
      GPR.Env.Initialize_Default_Project_Path
        (Self.Env.Project_Path, Target_Name => "");
      Self.Predefined_Project_Path :=
        new File_Array'(From_Path (+Get_Path (Self.Env.Project_Path)));
      Self.IDE_Mode := IDE_Mode;
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Tree : in out Project_Tree'Class;
      Env  : Project_Environment_Access) is
   begin
      if Tree.Data = null then
         Tree.Data := new Project_Tree_Data (Is_Aggregated => False);

         if Env = null then
            Initialize (Tree.Data.Env);
         else
            Tree.Data.Env := Env;
         end if;
      end if;

      if Tree.Data.Tree = null then
         Tree.Data.Tree := new Project_Node_Tree_Data;
      end if;

      GPR.Tree.Initialize (Tree.Data.Tree);

      if Tree.Data.View = null then
         Tree.Data.View := new GPR.Project_Tree_Data;
      end if;

      GPR.Initialize (Tree.Data.View);
   end Reset;

   -----------------------------
   -- Invalidate_Gnatls_Cache --
   -----------------------------

   procedure Invalidate_Gnatls_Cache (Self : in out Project_Environment) is
   begin
      Free (Self.Gnatls);
   end Invalidate_Gnatls_Cache;

   ------------------------
   -- Set_Default_Gnatls --
   ------------------------

   procedure Set_Default_Gnatls
     (Self         : in out Project_Environment;
      Gnatls       : String)
   is
   begin
      Free (Self.Default_Gnatls);
      Self.Default_Gnatls := new String'(Gnatls);
   end Set_Default_Gnatls;

   ----------------------------
   -- Set_Target_And_Runtime --
   ----------------------------

   procedure Set_Target_And_Runtime
      (Self    : in out Project_Environment;
       Target  : String := "";
       Runtime : String := "") is
   begin
      Free (Self.Forced_Target);
      Free (Self.Forced_Runtime);
      if Target /= "" then
         Self.Forced_Target := new String'(Target);
      end if;
      if Runtime /= "" then
         Self.Forced_Runtime := new String'(Runtime);
      end if;
   end Set_Target_And_Runtime;

   ----------------------------------------
   -- Set_Target_And_Runtime_From_Config --
   ----------------------------------------

   procedure Set_Target_And_Runtime_From_Config
     (Self : in out Project_Environment)
   is
      Config_Project_Node : GPR.Project_Node_Id;
      Project_Node_Tree   : GPR.Project_Node_Tree_Ref :=
        new Project_Node_Tree_Data;
      Project_Tree        : Project_Tree_Ref :=
        new GPR.Project_Tree_Data (Is_Root_Tree => True);
      Config              : Project_Id;
      Success             : Boolean;

      function Get_Config_Attribute_Value
        (Config_File  : Project_Id;
         Project_Tree : Project_Tree_Ref;
         Name         : String;
         Index        : String := "";
         Pack         : String := "") return String;
      --  Retruns the value of specified attribute from configuration project
      --  or an empty string if corresponding attribute is not found.
      --  Name, Index and Pack parameters are not case-sensitive.

      function Gnatls_From_CGPR
        (Runtime : String;
         Gcc     : String) return String;
      --  Constructs call to gnatls based on attributes from configuration
      --  project.

      --------------------------------
      -- Get_Config_Attribute_Value --
      --------------------------------

      function Get_Config_Attribute_Value
        (Config_File  : Project_Id;
         Project_Tree : Project_Tree_Ref;
         Name         : String;
         Index        : String := "";
         Pack         : String := "") return String
      is
         Shared             : constant Shared_Project_Tree_Data_Access :=
           Project_Tree.Shared;

         Conf_Decl : Declarations;
         Conf_Attr_Id       : Variable_Id;
         Conf_Attr          : Variable;

         Conf_Array_Id      : Array_Id;
         Conf_Array         : Array_Data;
         Conf_Array_Elem_Id : Array_Element_Id;
         Conf_Array_Elem    : Array_Element;

         Conf_Pack_Id : Package_Id;
         Conf_Pack    : Package_Element;

         function "=" (L : Name_Id; R : String) return Boolean is
           (To_Lower (Get_Name_String (L)) = To_Lower (R));
      begin
         if Config_File = GPR.No_Project then
            return "";
         end if;

         Conf_Decl := Config_File.Decl;

         if Pack /= "" then
            Conf_Pack_Id := Conf_Decl.Packages;
            while Conf_Pack_Id /= No_Package loop
               Conf_Pack := Shared.Packages.Table (Conf_Pack_Id);

               exit when Conf_Pack.Name = Pack;

               Conf_Pack_Id := Conf_Pack.Next;
            end loop;

            if Conf_Pack_Id = No_Package then
               return "";
            else
               Conf_Decl := Conf_Pack.Decl;
            end if;
         end if;

         if Index = "" then

            Conf_Attr_Id := Conf_Decl.Attributes;

            while Conf_Attr_Id /= GPR.No_Variable loop
               Conf_Attr := Shared.Variable_Elements.Table (Conf_Attr_Id);

               if not Conf_Attr.Value.Default then
                  if Conf_Attr.Name = Name then
                     if Conf_Attr.Value.Kind = Single then
                        return Get_Name_String (Conf_Attr.Value.Value);
                     end if;
                  end if;

               end if;

               Conf_Attr_Id := Conf_Attr.Next;
            end loop;

         else

            Conf_Array_Id := Conf_Decl.Arrays;

            while Conf_Array_Id /= No_Array loop
               Conf_Array := Shared.Arrays.Table (Conf_Array_Id);

               if Conf_Array.Name = Name then
                  Conf_Array_Elem_Id := Conf_Array.Value;
                  while Conf_Array_Elem_Id /= No_Array_Element loop
                     Conf_Array_Elem :=
                       Shared.Array_Elements.Table (Conf_Array_Elem_Id);

                     if Conf_Array_Elem.Index = Index then
                        return Get_Name_String (Conf_Array_Elem.Value.Value);
                     end if;

                     Conf_Array_Elem_Id := Conf_Array_Elem.Next;
                  end loop;
               end if;

               Conf_Array_Id := Conf_Array.Next;
            end loop;
         end if;

         return "";
      end Get_Config_Attribute_Value;

      ----------------------
      -- Gnatls_From_CGPR --
      ----------------------

      function Gnatls_From_CGPR
        (Runtime : String;
         Gcc     : String) return String
      is
         Idx : Integer;
      begin

         if Gcc = "" then
            return
              "gnatls -v"
              & (if Runtime = "" then "" else "--RTS=" & Runtime);
         else
            Idx := Index (Gcc, "gcc", Backward);

            if Idx > Gcc'First and then Idx = Gcc'Last - 2 then
               return
                 Gcc (Gcc'First .. Idx - 1)
                 & "gnatls -v"
                 & (if Runtime = "" then "" else " --RTS=" & Runtime);
            end if;
         end if;

         return "gnatls -v";
      end Gnatls_From_CGPR;

   begin
      Trace (Me, "Set_Target_And_Runtime_From_Config");
      if Self.Config_File = GNATCOLL.VFS.No_File
        or else not Self.Config_File.Is_Regular_File
      then
         Trace (Me, "Config file not found");
         return;
      end if;
      GPR.Snames.Initialize;
      GPR.Attr.Initialize;
      GPR.Tree.Initialize (Project_Node_Tree);
      GPR.Initialize (Project_Tree);

      GPR.Part.Parse
        (In_Tree           => Project_Node_Tree,
         Project           => Config_Project_Node,
         Project_File_Name => Self.Config_File.Display_Full_Name,
         Packages_To_Check => GPR.All_Packages,
         Is_Config_File    => True,
         Env               => Self.Env);

      if not Present (Config_Project_Node) then
         Trace (Me, "Cannot parse config project");
         return;
      end if;

      Proc.Process_Project_Tree_Phase_1
        (In_Tree                => Project_Tree,
         Project                => Config,
         Packages_To_Check      => GPR.All_Packages,
         Success                => Success,
         From_Project_Node      => Config_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Env                    => Self.Env,
         Reset_Tree             => False,
         On_New_Tree_Loaded     => null);

      if not Success then
         Trace (Me, "Cannot process config project");
         return;
      end if;

      declare
         CGPR_Target  : constant String :=
           Get_Config_Attribute_Value (Config, Project_Tree, "target");
         CGPR_Runtime : constant String :=
           Get_Config_Attribute_Value
             (Config, Project_Tree, "Runtime_Dir", "ADA");
         CGPR_GCC     : constant String :=
           Get_Config_Attribute_Value
             (Config, Project_Tree, "Driver", "ADA", "Compiler");
      begin
         Free (Self.Forced_Target);
         Free (Self.Forced_Runtime);
         if CGPR_Target /= "" then
            Self.Forced_Target := new String'(CGPR_Target);
         end if;
         if CGPR_Runtime /= "" then
            Self.Forced_Runtime := new String'(CGPR_Runtime);
         end if;
         Trace (Me, CGPR_GCC);

         Trace (Me,
                Gnatls_From_CGPR (CGPR_Runtime, CGPR_GCC));

         Self.Set_Default_Gnatls
           (Gnatls_From_CGPR (CGPR_Runtime, CGPR_GCC));
      end;
      Free (Project_Tree);
      Free (Project_Node_Tree);
   end Set_Target_And_Runtime_From_Config;

   ------------------------------------
   -- Set_Path_From_Gnatls_Attribute --
   ------------------------------------

   function Set_Path_From_Gnatls_Attribute
     (Project         : Project_Id;
      Tree            : Project_Tree'Class;
      Errors          : Error_Report := null)
      return Boolean
   is
      P       : Package_Id;
      Value   : Variable_Value;
      GNAT_Version : GNAT.Strings.String_Access;

      Shared : constant Shared_Project_Tree_Data_Access :=
         Tree.Data.View.Shared;

      Unset : constant String := "";

      function Get_Value_Of_Target (Project : Project_Id) return String;
      --  Look for the value of Target attribute in given project or projects
      --  extended by it recursively.

      function Get_Value_Of_Target (Project : Project_Id) return String is
         Elem : constant Variable_Value :=
           Value_Of (Get_String ("target"), Project.Decl.Attributes, Shared);
      begin
         if Elem = Nil_Variable_Value
           or else Elem.Default
           or else Elem.Value = No_Name
         then
            if Project.Extends = GPR.No_Project then
               return Value_Of (Nil_Variable_Value, Unset);
            else
               return Get_Value_Of_Target (Project.Extends);
            end if;
         else
            return Value_Of (Elem, Unset);
         end if;
      end Get_Value_Of_Target;

      Target : constant String :=
        (if Tree.Data.Env.Forced_Target /= null then
            Tree.Data.Env.Forced_Target.all
         else
            Get_Value_Of_Target (Project));

      N_Target : constant String := Normalize_Target_Name (Target);

      function Get_Value_Of_Runtime (Project : Project_Id) return String;
      --  Look for the value of Runtime attribute in given project or projects
      --  extended by it recursively.

      function Get_Value_Of_Runtime (Project : Project_Id) return String is
         Elem : constant Array_Element_Id := Value_Of
           (Get_String ("runtime"), Project.Decl.Arrays, Shared);

         function Filter_Default (S : String) return String is
           (if S = "default" then Unset else S);
         --  Unlike gprconfig, gnatls cannot process --RTS=default, so we need
         --  to replace it with empty value.
      begin
         if Elem = No_Array_Element then
            if Project.Extends = GPR.No_Project then
               return Filter_Default (Value_Of (Nil_Variable_Value, Unset));
            else
               return Filter_Default (Get_Value_Of_Runtime (Project.Extends));
            end if;
         else
            return Filter_Default
              (Value_Of
                 (Value_Of
                      (Index    => Get_String ("ada"),
                       In_Array => Elem,
                       Shared   => Shared),
                  Unset));
         end if;
      end Get_Value_Of_Runtime;

      Runtime : constant String :=
         (if Tree.Data.Env.Forced_Runtime /= null then
            Tree.Data.Env.Forced_Runtime.all
          else
            Get_Value_Of_Runtime (Project));

      function Default_Gnatls return String;
      --  Compute the default 'gnatls' command to spawn

      function Default_Gnatls return String is
         No_Prefix : Boolean := False;
      begin
         if Tree.Data.Env.Config_File.Is_Regular_File
           and then Tree.Data.Env.Default_Gnatls /= null
         then
            return Tree.Data.Env.Default_Gnatls.all;
         end if;

         for Tgt of Host_Targets_List loop
            if N_Target = Tgt then
               No_Prefix := True;
               exit;
            end if;
         end loop;

         if Runtime /= Unset
            or else Target /= Unset
         then
            return
              (if Target /= Unset
               and then not No_Prefix then Target & '-' else "")
               & "gnatls"
               & (if Runtime /= Unset then " --RTS=" & Runtime else "");
         else
            return Tree.Data.Env.Default_Gnatls.all;
         end if;
      end Default_Gnatls;

      function Process_Gnatls (Gnatls : String) return Boolean;
      function Process_Gnatls (Gnatls : String) return Boolean is
      begin
         if Tree.Data.Env.Gnatls = null or else
           (Tree.Data.Env.Gnatls.all /= Gnatls and then
            Tree.Data.Env.Gnatls.all /= No_Gnatls)
         then
            Tree.Data.Env.Set_Path_From_Gnatls
               (Gnatls       => Gnatls,
                GNAT_Version => GNAT_Version,
                Errors       => Errors);
            Free (GNAT_Version);
            return True;
         end if;
         return False;
      end Process_Gnatls;

   begin
      P := Value_Of
        (Name_Ide,
         In_Packages => Project.Decl.Packages,
         Shared      => Shared);
      if P = No_Package then
         Trace (Me, "No package IDE, no gnatlist attribute");
         return Process_Gnatls (Default_Gnatls);
      else
         --  Do we have a gnatlist attribute ?
         Value := Value_Of
           (Get_String ("gnatlist"),
            Tree.Data.View.Shared.Packages.Table (P).Decl.Attributes, Shared);

         if Value = Nil_Variable_Value then
            Trace (Me, "No attribute IDE'gnatlist");
            return Process_Gnatls (Default_Gnatls);
         else
            declare
               Gnatls : constant String := Get_Name_String (Value.Value);
            begin
               if Gnatls = "" then
                  return Process_Gnatls (Default_Gnatls);
               else
                  if Runtime /= Unset or else Target /= Unset then
                     Trace (Me, "Error, IDE'Gnatlist attribute cannot be set"
                        & " when Runtime or Target is also set");
                     return Process_Gnatls (Default_Gnatls);
                  end if;

                  return Process_Gnatls (Gnatls);
               end if;
            end;
         end if;
      end if;
   end Set_Path_From_Gnatls_Attribute;

   ------------------
   -- Spawn_Gnatls --
   ------------------

   procedure Spawn_Gnatls
     (Self         : Project_Environment;
      Fd           : out Process_Descriptor_Access;
      Gnatls_Args  : Argument_List_Access;
      Errors       : Error_Report)
   is
      Gnatls_Path : constant Virtual_File :=
        Locate_On_Path (+Gnatls_Args (Gnatls_Args'First).all);
   begin
      if Gnatls_Path = GNATCOLL.VFS.No_File then
         Trace (Me, "Could not locate exec " &
                  Gnatls_Args (Gnatls_Args'First).all);

         if Errors /= null then
            Errors ("Could not locate exec " &
                      Gnatls_Args (Gnatls_Args'First).all);
         end if;
      else
         Trace (Me, "Spawning " & (+Gnatls_Path.Full_Name));
         if Self.TTY_Process_Descriptor_Disabled then
            Fd := new Process_Descriptor;
         else
            Fd := new TTY_Process_Descriptor;
         end if;
         Non_Blocking_Spawn
           (Fd.all,
            +Gnatls_Path.Full_Name,
            Gnatls_Args (Gnatls_Args'First + 1 .. Gnatls_Args'Last),
            Buffer_Size => 0, Err_To_Out => True);
      end if;
   end Spawn_Gnatls;

   -----------------
   -- Gnatls_Host --
   -----------------

   function Gnatls_Host
     (Self : Project_Environment) return String
   is
      pragma Unreferenced (Self);
   begin
      return Local_Host;
   end Gnatls_Host;

   --------------------------
   -- Set_Path_From_Gnatls --
   --------------------------

   procedure Set_Path_From_Gnatls
     (Self         : in out Project_Environment;
      Gnatls       : String;
      GNAT_Version : out GNAT.Strings.String_Access;
      Errors       : Error_Report := null)
   is
      Gnatls_Args    : Argument_List_Access :=
        Argument_String_To_List (Gnatls & " -v");

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Process_Descriptor'Class, Process_Descriptor_Access);

      Success : Boolean := True;
      Fd      : Process_Descriptor_Access;

   begin
      if Self.Default_Gnatls /= null
        and then Self.Default_Gnatls.all = No_Gnatls
      then
         Self.Gnatls := new String'(Self.Default_Gnatls.all);
         Trace (Me, "Gnatls should not be invoked");
         return;
      end if;

      if Self.Gnatls /= null
        and then Self.Gnatls.all = Gnatls
      then
         Trace (Me, "Gnatls was already run with same arguments: " & Gnatls);
         return;
      end if;

      Free (Self.Gnatls);
      Self.Gnatls := new String'(Gnatls);

      if Active (Me) then
         Increase_Indent (Me, "Executing " & Gnatls & " -v");
      end if;

      begin
         Spawn_Gnatls
           (Project_Environment'Class (Self), Fd, Gnatls_Args, Errors);
      exception
         when others =>
            Trace (Me, "Could not execute " & Gnatls_Args (1).all);
            if Errors /= null then
               Errors ("Could not execute " & Gnatls_Args (1).all);
            end if;
            Success := False;
      end;

      if not Success then
         Trace (Me, "Could not compute predefined paths");
         if Errors /= null then
            Errors
              ("Could not compute predefined paths for this project.");
            Errors
              ("Subprojects might be incorrectly loaded, please make " &
               "sure they are in your ADA_PROJECT_PATH");
         end if;

         if Active (Me) then
            Decrease_Indent (Me);
         end if;

         return;
      end if;

      if Fd /= null then
         declare
            S : constant String := GNATCOLL.Utils.Get_Command_Output (Fd);
         begin
            Trace (Me, "Output of gnatls is " & S);

            if S = "" and Errors /= null then
               Errors ("The output from '" & Gnatls & "-v' is empty");
            end if;

            Set_Path_From_Gnatls_Output
              (Self,
               Output       => S,
               GNAT_Version => GNAT_Version,
               Host         =>
                 Gnatls_Host (Project_Environment'Class (Self)));
         end;

         Unchecked_Free (Fd);
      end if;

      Free (Gnatls_Args);

      if Active (Me) then
         Decrease_Indent (Me);
      end if;
   end Set_Path_From_Gnatls;

   ---------------------------------
   -- Set_Path_From_Gnatls_Output --
   ---------------------------------

   procedure Set_Path_From_Gnatls_Output
     (Self         : in out Project_Environment;
      Output       : String;
      Host         : String := GNATCOLL.VFS.Local_Host;
      GNAT_Version : out GNAT.Strings.String_Access)
   is
      type Path_Context is (None, Source_Path, Object_Path, Project_Path);
      Context : Path_Context := None;

      Current         : GNATCOLL.VFS.File_Array_Access :=
                          new File_Array'(1 .. 0 => <>);
      Object_Path_Set : Boolean := False;

      procedure Add_Directory (S : String);
      --  Add S to the search path.
      --  If Source_Path is True, the source path is modified.
      --  Otherwise, the object path is modified.

      procedure Set_Context (New_Context : Path_Context);
      --  Change the context

      -------------------
      -- Add_Directory --
      -------------------

      procedure Add_Directory (S : String) is
         Dir : Virtual_File;
      begin
         if S = "" then
            return;

         elsif S = "<Current_Directory>" then
            if not Object_Path_Set then
               --  Do not include "." in the default source/object paths: when
               --  the user is compiling, it would represent the object
               --  directory, when the user is searching file it would
               --  represent whatever the current directory is at that point,
               --  ...
               return;
            else
               Dir := Create_From_Base (".");
               Ensure_Directory (Dir);
               Append (Current, Dir);
            end if;

         else
            Dir := To_Local (Create (+S, Host));
            Append (Current, Dir);
         end if;
      end Add_Directory;

      -----------------
      -- Set_Context --
      -----------------

      procedure Set_Context (New_Context : Path_Context) is
      begin
         case Context is
            when None =>
               null;

            when Source_Path =>
               Self.Set_Predefined_Source_Path (Current.all);

            when Object_Path =>
               Object_Path_Set := True;
               Self.Set_Predefined_Object_Path (Current.all);

            when Project_Path =>
               Self.Set_Predefined_Project_Path (Current.all);
         end case;

         if Active (Me) and then Context /= None then
            Trace (Me, "Set " & Context'Img & " from gnatls to:");
            for J in Current'Range loop
               Trace (Me, "  " & Current (J).Display_Full_Name);
            end loop;
         end if;

         Context := New_Context;
         Unchecked_Free (Current);

         if Context /= None then
            Current := new File_Array'(1 .. 0 => <>);
         end if;
      end Set_Context;

      F, L : Natural;

   begin
      F := Output'First;
      Skip_Blanks (Output, F);

      L := EOL (Output (F .. Output'Last));

      declare
         S : constant String := Strip_CR (Output (F .. L - 1));
      begin
         GNAT_Version := new String'(S (S'First + 7 .. S'Last));
         Project_Environment'Class (Self).Set_GNAT_Version (GNAT_Version.all);
      end;

      F := L + 1;

      while F <= Output'Last loop
         L := EOL (Output (F .. Output'Last));

         if GU.Starts_With (Output (F .. L - 1), "Source Search Path:") then
            Set_Context (Source_Path);

         elsif GU.Starts_With (Output (F .. L - 1), "Object Search Path:") then
            Set_Context (Object_Path);

         elsif GU.Starts_With (Output (F .. L - 1), "Project Search Path:")
         then
            Set_Context (Project_Path);

         elsif Context /= None then
            Add_Directory
              (Trim (Strip_CR (Output (F .. L - 1)), Ada.Strings.Left));
         end if;

         F := L + 1;
      end loop;

      Set_Context (None);
   end Set_Path_From_Gnatls_Output;

   -------------------
   -- Internal_Load --
   -------------------

   procedure Internal_Load
     (Tree                   : in out Project_Tree'Class;
      Root_Project_Path      : GNATCOLL.VFS.Virtual_File;
      Errors                 : Projects.Error_Report;
      Report_Syntax_Errors   : Boolean;
      Project                : out Project_Node_Id;
      Packages_To_Check      : GNAT.Strings.String_List_Access := All_Packs;
      Recompute_View         : Boolean := True;
      Test_With_Missing_With : Boolean := True;
      Report_Missing_Dirs    : Boolean := True;
      Implicit_Project       : Boolean)
   is
      Block_Me : constant Block_Trace_Handle := Create (Me);

      procedure On_Error is new Mark_Project_Error (Tree);
      --  Any error while parsing the project marks it as incomplete, and
      --  prevents direct edition of the project.

      procedure Fail (S : String);
      --  Replaces Osint.Fail

      procedure Filter_Reload_Warnings (S : String);
      --  When loading a new project on top of an already loaded one, and both
      --  those projects have same name of external, but those externals
      --  correspond to different set of values, gprlib issues a warning.
      --  This warning is harmless and does not prevent the loading of the new
      --  project. However we can not clear the externals table since there are
      --  cases when we do want to store the values of all externals.
      --  So we just filter out such warnings.

      procedure Clean_Up_Node_Tree
        (Node_Tree    : GPR.Project_Node_Tree_Ref;
         Tree         : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project      : Project_Id);
      --  Simple callback to free unused node trees that may be created for
      --  aggregated projects.

      ------------------------
      -- Clean_Up_Node_Tree --
      ------------------------

      procedure Clean_Up_Node_Tree
        (Node_Tree    : GPR.Project_Node_Tree_Ref;
         Tree         : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project      : Project_Id)
      is
         pragma Unreferenced (Tree, Project_Node, Project);
         Local_Node_Tree : GPR.Project_Node_Tree_Ref := Node_Tree;
      begin
         Free (Local_Node_Tree);
      end Clean_Up_Node_Tree;

      ----------------------------
      -- Filter_Reload_Warnings --
      ----------------------------

      procedure Filter_Reload_Warnings (S : String) is
         Pattern   : constant String := """ is illegal for typed string """;
      begin
         if Errors = null then
            Trace (Me, "calling output wrapper when Errors callback not set");
            return;
         end if;

         if Index (S, Pattern) = 0 then
            Errors (S);
         end if;
      end Filter_Reload_Warnings;

      ----------
      -- Fail --
      ----------

      procedure Fail (S : String) is
      begin
         if Report_Syntax_Errors and then Errors /= null then
            Errors (S);
         end if;
      end Fail;

      Predefined_Path : constant String :=
        +To_Path (Predefined_Project_Path (Tree.Data.Env.all));

      Errout_Handling : GPR.Part.Errout_Mode := GPR.Part.Always_Finalize;
   begin
      Traces.Assert (Me, Tree.Data /= null, "Tree data initialized");

      Reset (Tree, Tree.Data.Env);

      Trace (Me, "project path is " & Predefined_Path);
      Initialize_Empty (Tree.Data.Env.Env.Project_Path);
      GPR.Env.Set_Path (Tree.Data.Env.Env.Project_Path, Predefined_Path);

      Project := Empty_Project_Node;

      --  Make sure errors are reinitialized before load
      GPR.Err.Initialize;

      if Test_With_Missing_With then
         Errout_Handling := GPR.Part.Never_Finalize;
      end if;

      if Errors = null then
         --  We do not want to loose the output in the wrapper if the callback
         --  is not specified.
         GPR.Output.Cancel_Special_Output;
      else
         if Tree.Data.Env.IDE_Mode then
            GPR.Output.Set_Special_Output
              (Filter_Reload_Warnings'Unrestricted_Access);
         else
            GPR.Output.Set_Special_Output (GPR.Output.Output_Proc (Errors));
         end if;
      end if;
      GPR.Com.Fail := Fail'Unrestricted_Access;

      Tree.Data.Root := No_Project;

      Override_Flags
        (Tree.Data.Env.Env,
         Create_Flags
           (On_Error'Unrestricted_Access,
            Report_Missing_Dirs => not Report_Missing_Dirs,
            Ignore_Missing_With => Test_With_Missing_With));

      GPR.Part.Parse
        (Tree.Data.Tree, Project,
         +Root_Project_Path.Full_Name,
         Packages_To_Check => Packages_To_Check,
         Errout_Handling   => Errout_Handling,
         Store_Comments    => True,
         Is_Config_File    => False,
         Env               => Tree.Data.Env.Env,
         Current_Directory => Get_Current_Dir,
         Implicit_Project  => Implicit_Project);

      if not Active (Me_Aggregate_Support)
        and then Project /= Empty_Project_Node
        and then Project_Qualifier_Of (Project, Tree.Data.Tree) =
        GPR.Aggregate
      then
         Trace (Me, "Aggregate projects are not supported");
         Fail ("Aggregate projects are not supported");
         Project := Empty_Project_Node;
         GPR.Com.Fail := null;
         GPR.Output.Cancel_Special_Output;
         return;
      end if;

      if Project /= Empty_Project_Node
        and then Tree.Data.Tree.Incomplete_With
      then
         Trace (Me, "Could not find some with-ed projects");

         --  Some "with" were found that could not be resolved. Check whether
         --  the user has specified a "gnatlist" switch. For this, we need to
         --  do phase1 of the processing (i.e. not look for sources).

         declare
            Success : Boolean;
            Tmp_Prj : Project_Id;
            Dummy   : Boolean;
         begin
            Tree.Data.Projects.Clear;
            GPR.Proc.Process_Project_Tree_Phase_1
              (In_Tree                => Tree.Data.View,
               Project                => Tmp_Prj,
               Packages_To_Check      => Packages_To_Check,
               Success                => Success,
               From_Project_Node      => Project,
               From_Project_Node_Tree => Tree.Data.Tree,
               Env                    => Tree.Data.Env.Env,
               Reset_Tree             => True,
               On_New_Tree_Loaded     =>
                 Clean_Up_Node_Tree'Unrestricted_Access);

            if not Success or else Tmp_Prj = null then
               Trace (Me, "Processing phase 1 failed");
               Project := Empty_Project_Node;
            else
               Trace (Me, "Looking for IDE'gnatlist attribute");
               Dummy := Set_Path_From_Gnatls_Attribute
                 (Tmp_Prj, Tree, Fail'Unrestricted_Access);
            end if;

            --  Reparse the tree so that errors are reported as usual
            --  (or not if the new project path solves the issue).

            Override_Flags
              (Tree.Data.Env.Env,
               Create_Flags
                 (On_Error'Unrestricted_Access,
                  Report_Missing_Dirs => Report_Missing_Dirs,
                  Ignore_Missing_With => False));

            Trace (Me, "Parsing project tree a second time");

            Internal_Load
              (Tree                   => Tree,
               Root_Project_Path      => Root_Project_Path,
               Errors                 => Errors,
               Report_Syntax_Errors   => Report_Syntax_Errors,
               Project                => Project,
               Recompute_View         => Recompute_View,
               Packages_To_Check      => Packages_To_Check,
               Test_With_Missing_With => False,
               Report_Missing_Dirs    => Report_Missing_Dirs,
               Implicit_Project       => Implicit_Project);

            GPR.Com.Fail := null;
            GPR.Output.Cancel_Special_Output;

            return;
         end;

      elsif Project = Empty_Project_Node
        and then Test_With_Missing_With
      then
         --  We had error, but we might be missing the one for missing withs.
         --  So we do a second parsing to make sure these error messages are
         --  there.

         Trace (Me, "Had error messages, reparsing to include missing withs");
         Override_Flags
           (Tree.Data.Env.Env,
            Create_Flags
              (On_Error'Unrestricted_Access,
               Report_Missing_Dirs => Report_Missing_Dirs,
               Ignore_Missing_With => False));
         Internal_Load
           (Tree                   => Tree,
            Root_Project_Path      => Root_Project_Path,
            Errors                 => Errors,
            Report_Syntax_Errors   => Report_Syntax_Errors,
            Project                => Project,
            Recompute_View         => Recompute_View,
            Packages_To_Check      => Packages_To_Check,
            Test_With_Missing_With => False,
            Report_Missing_Dirs    => Report_Missing_Dirs,
            Implicit_Project       => Implicit_Project);

         GPR.Com.Fail := null;
         GPR.Output.Cancel_Special_Output;

         return;

      elsif Test_With_Missing_With then
         Trace (Me, "Project parsed with success");

         --  We correctly parsed the project, but should finalize anyway
         if Report_Syntax_Errors then
            GPR.Err.Finalize;
         else
            GPR.Err.Initialize;
         end if;
      end if;

      --  Should we reprocess with a different predefined path ?
      --  We need to do a full reparse here (not just recompute the view),
      --  because changing gnatls might change the search path for projects,
      --  and thus the way the with statements are resolved.

      declare
         Success : Boolean;
         Tmp_Prj : Project_Id;
         Dummy   : Boolean;
      begin
         Trace (Me, "Checking whether the gnatls attribute has changed");

         --  Just clearing the projects htable is not enough, the memory will
         --  not be freed unless we set corresponding tree fields to null.
         --  Then finalize recognizes those project instances as useless
         --  and cleans them up.
         declare
            Cur : Project_Htables.Cursor := Tree.Data.Projects.First;
         begin
            while Cur /= Project_Htables.No_Element loop
               Project_Htables.Element (Cur).Data.Tree := null;
               Next (Cur);
            end loop;
         end;
         Tree.Data.Projects.Clear;
         GPR.Proc.Process_Project_Tree_Phase_1
           (In_Tree                => Tree.Data.View,
            Project                => Tmp_Prj,
            Packages_To_Check      => Packages_To_Check,
            Success                => Success,
            From_Project_Node      => Project,
            From_Project_Node_Tree => Tree.Data.Tree,
            Env                    => Tree.Data.Env.Env,
            Reset_Tree             => True,
            On_New_Tree_Loaded     => Clean_Up_Node_Tree'Unrestricted_Access);

         if Success
           and then Tmp_Prj /= null
           and then Set_Path_From_Gnatls_Attribute
               (Tmp_Prj, Tree, Fail'Unrestricted_Access)
         then
            Trace (Me, "load again with proper path");
            Internal_Load
              (Tree                   => Tree,
               Root_Project_Path      => Root_Project_Path,
               Errors                 => Errors,
               Report_Syntax_Errors   => Report_Syntax_Errors,
               Project                => Project,
               Recompute_View         => Recompute_View,
               Packages_To_Check      => Packages_To_Check,
               Test_With_Missing_With => False,
               Report_Missing_Dirs    => Report_Missing_Dirs,
               Implicit_Project       => Implicit_Project);

            GPR.Com.Fail := null;
            GPR.Output.Cancel_Special_Output;

            return;
         end if;
      end;

      Override_Flags (Tree.Data.Env.Env, Create_Flags (null));

      if Project /= Empty_Project_Node then
         Tree.Data.Root := Tree.Instance_From_Node (Tree, Project);

         --  Create the project instances, so that we can use the
         --  project_iterator (otherwise Current cannot return a project_type).
         --  These instances, for now, will have now view associated

         Create_Project_Instances
           (Tree, Tree, With_View => False);

         Tree.Set_Status (From_File);

         if Report_Syntax_Errors then
            --  Some errors might come form GPR.Part.Parse but only from
            --  GPR.Proc.Process_Project_Tree_Phase_1, like undefined
            --  externals. We need to show them.
            GPR.Err.Finalize;
         end if;

         GPR.Com.Fail := null;
         GPR.Output.Cancel_Special_Output;

         --  For future recomputations of the view we want to keep the same
         --  flag over and over again.
         Tree.Data.Env.Report_Missing_Dirs := Report_Missing_Dirs;

         --  For future recomputations of view we also want to keep the list
         --  of packages to check, but in case it is not a predefined one,
         --  we need a hard copy, since users might free the list right after
         --  the loading.
         if Packages_To_Check in No_Packs | All_Packs then
            Tree.Data.Env.Packages_To_Check := Packages_To_Check;
         else
            Tree.Data.Env.Packages_To_Check   :=
              new String_List (Packages_To_Check'Range);
            for I in Packages_To_Check'Range loop
               Tree.Data.Env.Packages_To_Check (I) :=
                 new String'(Packages_To_Check (I).all);
            end loop;
         end if;

         if Recompute_View then
            Tree.Recompute_View (Errors => Errors);
         end if;
      end if;

      GPR.Com.Fail := null;
      GPR.Output.Cancel_Special_Output;

   exception
      when Invalid_Project =>
         GPR.Com.Fail := null;
         GPR.Output.Cancel_Special_Output;
         raise;

      when E : others =>
         Trace (Me, E);
         GPR.Com.Fail := null;
         GPR.Output.Cancel_Special_Output;
         raise;
   end Internal_Load;

   ----------------
   -- Reset_View --
   ----------------

   procedure Reset_View (Tree : Project_Tree'Class) is
   begin
      if not Tree.Data.Is_Aggregated then
         Clean_Up (Tree.Data.Sources);
         Clean_Up (Tree.Data.Objects_Basename);
      end if;

      Tree.Data.Directories.Clear;
      Unchecked_Free (Tree.Data.Env.Scenario_Variables);
      Unchecked_Free (Tree.Data.Env.Untyped_Variables);
   end Reset_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Self   : in out Project_Tree;
      Errors : Projects.Error_Report := null)
   is
      Block_Me : constant Block_Trace_Handle := Create (Me);

      Actual_Config_File : Project_Node_Id := Empty_Project_Node;
      Actual_Config_File_Tree : GPR.Project_Node_Tree_Ref := null;
      --  The config file that was used (and possibly augmented by custom
      --  naming schemes set in Register_Default_Language_Extension)

      procedure Add_Default_GNAT_Naming_Scheme
        (Config_File  : in out GPR.Project_Node_Id;
         Project_Tree : GPR.Project_Node_Tree_Ref);
      --  A hook that will create a new config file (in memory), used for
      --  Get_Or_Create_Configuration_File and Process_Project_And_Apply_Config
      --  and add the default GNAT naming scheme to it. Nothing is done if the
      --  config_file already exists, to avoid overriding what the user might
      --  have put in there.

      procedure Add_GPS_Naming_Schemes_To_Config_File
        (Config_File  : in out Project_Node_Id;
         Project_Tree : GPR.Project_Node_Tree_Ref);
      --  Add the naming schemes defined in GPS's configuration files to the
      --  configuration file (.cgpr) used to parse the project.

      procedure On_Error is new Mark_Project_Error (Self);
      --  Any error while processing the project marks it as incomplete, and
      --  prevents direct edition of the project.

      procedure Initialize_Source_Records;
      --  Compute extra information for each source file, in particular whether
      --  it is a separate (as opposed to a body). This might require extra
      --  parsing of the source file in some cases.

      procedure On_New_Tree_Loaded
        (Node_Tree : GPR.Project_Node_Tree_Ref;
         Tree : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project : Project_Id);
      --  Creates project instances for given project tree.
      --  This is called once per aggregated project tree

      Undefined_Externals_Present : Boolean := False;
      procedure Catch_Undefined_Externals (S : String);
      --  Sets Undefined_Externals_Present to true if there is at least one
      --  error message about undefined externals when loading the project.
      --  This only works in IDE mode, since for other tools there is no way
      --  to change the Scenario Variables mid-loading and recompute view.

      ------------------------------------
      -- Add_Default_GNAT_Naming_Scheme --
      ------------------------------------

      procedure Add_Default_GNAT_Naming_Scheme
        (Config_File  : in out Project_Node_Id;
         Project_Tree : GPR.Project_Node_Tree_Ref)
      is
         Auto_Cgpr : constant String := "auto.cgpr";

         procedure Create_Attribute
           (Name  : Name_Id;
            Value : String;
            Index : String := "";
            Pkg   : Project_Node_Id := Empty_Project_Node);

         ----------------------
         -- Create_Attribute --
         ----------------------

         procedure Create_Attribute
           (Name  : Name_Id;
            Value : String;
            Index : String := "";
            Pkg   : Project_Node_Id := Empty_Project_Node)
         is
            Attr : Project_Node_Id;
            pragma Unreferenced (Attr);

            Expr   : Name_Id         := No_Name;
            Val    : Name_Id         := No_Name;
            Parent : Project_Node_Id := Config_File;

         begin
            if Index /= "" then
               Name_Len := Index'Length;
               Name_Buffer (1 .. Name_Len) := Index;
               Val := Name_Find;
            end if;

            if Pkg /= Empty_Project_Node then
               Parent := Pkg;
            end if;

            Name_Len := Value'Length;
            Name_Buffer (1 .. Name_Len) := Value;
            Expr := Name_Find;

            Attr := Create_Attribute
              (Tree       => Project_Tree,
               Prj_Or_Pkg => Parent,
               Name       => Name,
               Index_Name => Val,
               Kind       => GPR.Single,
               Value      => Create_Literal_String (Expr, Project_Tree));
         end Create_Attribute;

         --  Local variables

         Name     : Name_Id;
         Naming   : Project_Node_Id;
         Compiler : Project_Node_Id;

         --  Start of processing for Add_Default_GNAT_Naming_Scheme

      begin
         if Config_File = Empty_Project_Node then

            --  Create a dummy config file if none was found

            Name_Len := Auto_Cgpr'Length;
            Name_Buffer (1 .. Name_Len) := Auto_Cgpr;
            Name := Name_Find;

            --  An invalid project name to avoid conflicts with
            --  user-created ones.

            Name_Len := 5;
            Name_Buffer (1 .. Name_Len) := "_auto";

            Config_File :=
              Create_Project
                (In_Tree        => Project_Tree,
                 Name           => Name_Find,
                 Full_Path      => Path_Name_Type (Name),
                 Is_Config_File => True);

            --  Setup library support

            Create_Attribute (Name_Library_Support, "full");
            Create_Attribute (Name_Library_Auto_Init_Supported, "true");

            --  Declare an empty target

            Create_Attribute (Name_Target, "");

            --  Setup Ada support (Ada is the default language here, since this
            --  is only called when no config file existed initially, i.e. for
            --  gnatmake).

            Create_Attribute (Name_Default_Language, "ada");

            Compiler := Create_Package (Project_Tree, Config_File, "compiler");
            Create_Attribute
              (Name_Driver, "gcc", "ada", Pkg => Compiler);
            Create_Attribute
              (Name_Language_Kind, "unit_based", "ada", Pkg => Compiler);
            Create_Attribute
              (Name_Dependency_Kind, "ALI_File", "ada", Pkg => Compiler);

            Naming := Create_Package (Project_Tree, Config_File, "naming");
            Create_Attribute
              (Name_Spec_Suffix, ".ads", "ada",     Pkg => Naming);
            Create_Attribute
              (Name_Body_Suffix, ".adb", "ada",     Pkg => Naming);
            Create_Attribute
              (Name_Spec_Suffix, ".h", "c",     Pkg => Naming);
            Create_Attribute
              (Name_Body_Suffix, ".c", "c",     Pkg => Naming);
            Create_Attribute
              (Name_Spec_Suffix, ".hh", "c++",     Pkg => Naming);
            Create_Attribute
              (Name_Body_Suffix, ".cpp", "c++",     Pkg => Naming);
            Create_Attribute
              (Name_Dot_Replacement, "-",           Pkg => Naming);
            Create_Attribute
              (Name_Casing,          "lowercase",   Pkg => Naming);
         end if;
      end Add_Default_GNAT_Naming_Scheme;

      -------------------------------------------
      -- Add_GPS_Naming_Schemes_To_Config_File --
      -------------------------------------------

      procedure Add_GPS_Naming_Schemes_To_Config_File
        (Config_File  : in out Project_Node_Id;
         Project_Tree : GPR.Project_Node_Tree_Ref)
      is
         NS   : Naming_Scheme_Access := Self.Data.Env.Naming_Schemes;
         Attr : Project_Node_Id;
         Spec_Suffix, Body_Suffix, Obj_Suffix : Name_Id;
         Naming_Pkg, Compiler_Pkg : Project_Node_Id;
         pragma Unreferenced (Attr);
      begin
         if Config_File = Empty_Project_Node then
            --  Create a dummy config file if none was found. In that case we
            --  need to provide the Ada naming scheme as well

            Trace (Me, "Creating dummy configuration file");

            Add_Default_GNAT_Naming_Scheme (Config_File, Project_Tree);

            --  Pretend we support shared and static libs. Since we are not
            --  trying to build anyway, this isn't dangerous, and allows
            --  loading some libraries projects which otherwise we could not
            --  load.

            Attr := Create_Attribute
              (Tree       => Project_Tree,
               Prj_Or_Pkg => Config_File,
               Name       => Get_String ("library_support"),
               Kind       => Single,
               Value      => Create_Literal_String
                 (Tree => Project_Tree,
                  Str  => Get_String ("full")));
         end if;

         Spec_Suffix := Get_String ("spec_suffix");
         Body_Suffix := Get_String ("body_suffix");
         Obj_Suffix  := Get_String ("object_file_suffix");
         Naming_Pkg  := Create_Package
            (Tree    => Project_Tree,
             Project => Config_File,
             Pkg     => "naming");
         Compiler_Pkg  := Create_Package
            (Tree    => Project_Tree,
             Project => Config_File,
             Pkg     => "compiler");

         while NS /= null loop
            if NS.Default_Spec_Suffix.all /= Dummy_Suffix then
               Attr := Create_Attribute
                 (Tree               => Project_Tree,
                  Prj_Or_Pkg         => Naming_Pkg,
                  Kind               => Single,
                  Name               => Spec_Suffix,
                  Index_Name         => Get_String (NS.Language.all),
                  Value              => Create_Literal_String
                    (Tree  => Project_Tree,
                     Str   => Get_String (NS.Default_Spec_Suffix.all)));
            end if;

            if NS.Default_Body_Suffix.all /= Dummy_Suffix then
               Attr := Create_Attribute
                 (Tree               => Project_Tree,
                  Prj_Or_Pkg         => Naming_Pkg,
                  Kind               => Single,
                  Name               => Body_Suffix,
                  Index_Name         => Get_String (NS.Language.all),
                  Value              => Create_Literal_String
                    (Tree  => Project_Tree,
                     Str   => Get_String (NS.Default_Body_Suffix.all)));
            end if;

            if NS.Obj_Suffix /= null then
               Attr := Create_Attribute
                 (Tree               => Project_Tree,
                  Prj_Or_Pkg         => Compiler_Pkg,
                  Kind               => Single,
                  Name               => Obj_Suffix,
                  Index_Name         => Get_String (NS.Language.all),
                  Value              => Create_Literal_String
                    (Tree  => Project_Tree,
                     Str   => Get_String (NS.Obj_Suffix.all)));
            end if;

            NS := NS.Next;
         end loop;

         Actual_Config_File := Config_File;
         Actual_Config_File_Tree := Project_Tree;
      end Add_GPS_Naming_Schemes_To_Config_File;

      -------------------------------
      -- Initialize_Source_Records --
      -------------------------------

      procedure Initialize_Source_Records is

         procedure For_Sources
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Integer);

         -----------------
         -- For_Sources --
         -----------------

         procedure For_Sources
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Integer)
         is
            pragma Unreferenced (With_State);
            Iter : Source_Iterator := For_Each_Source
              (In_Tree => Tree, Project => Project);

            Src  : GPR.Source_Id;
         begin
            loop
               Src := Element (Iter);
               exit when Src = No_Source;

               --  ??? Calling Initialize_Source_Record computes additional
               --  information that we do not need at the moment, at the cost
               --  of a few system calls per source file. So instead we just
               --  duplicate the part that computes whether we have a separate
               --  unit.

               if False then
                  GPR.Util.Initialize_Source_Record (Src);
               else
                  if Src.Language.Config.Kind = Unit_Based
                    and then Src.Kind = Impl
                    and then GPR.Util.Is_Subunit (Src)
                  then
                     Src.Kind := Sep;
                  end if;
               end if;

               Next (Iter);
            end loop;
         end For_Sources;

         procedure For_Projects_Imported is new For_Every_Project_Imported
           (Integer, For_Sources);

         State : Integer := 0;
      begin
         For_Projects_Imported
           (By                 => Self.Root_Project.Data.View,
            Tree               => Self.Data.View,
            With_State         => State,
            Include_Aggregated => True,
            Imported_First     => False);
      end Initialize_Source_Records;

      View                    : Project_Id;
      Automatically_Generated : Boolean;
      Config_File_Path        : String_Access;
      Flags                   : Processing_Flags;
      Iter                    : Inner_Project_Iterator;
      Timestamp               : Time;

      procedure On_New_Tree_Loaded
        (Node_Tree    : GPR.Project_Node_Tree_Ref;
         Tree         : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project      : Project_Id)
      is
         pragma Unreferenced (Project);
         Path : constant Virtual_File :=
           Create (+Get_String (Path_Name_Of (Project_Node, Node_Tree)));

         T : Project_Tree'Class := Self;  --  copy the tag of self
         P : Project_Type;
         C    : constant Project_Htables.Cursor :=
           Self.Data.Projects.Find (Path);
      begin
         Trace (Me, "Loaded an aggregated tree");

         --  Recomputing the view might impact which aggregated projects are
         --  seen, so we need to create new project trees as needed.

         if Has_Element (C) and then Element (C).Data.Tree /= null then
            P := Element (C);
            if P.Data.Node /= Project_Node then
               --  The only way we can end up here is if the given aggregated
               --  tree is in fact a subtree of a previously processed
               --  aggregated tree. This means that we had already created all
               --  corresponding project instances and have nothing to do.
               return;
            end if;

            P.Data.Tree.View := Tree;
            T.Data := P.Data.Tree;   --  temporary
         else
            T.Data := new Project_Tree_Data'
              (Is_Aggregated => True,
               Env           => Self.Data.Env,
               Tree          => Node_Tree,
               View          => Tree,
               Status        => Self.Data.Status,
               Timestamp     => Self.Data.Timestamp,
               others        => <>);
            --  T.Data.Tree should be set before the instances can be created
            T.Data.Root := T.Instance_From_Node (Self, Project_Node);
         end if;

         Create_Project_Instances
           (T, Tree_For_Map => Self, With_View => False);
      end On_New_Tree_Loaded;

      -------------------------------
      -- Catch_Undefined_Externals --
      -------------------------------

      procedure Catch_Undefined_Externals (S : String) is
         Pattern   : constant String := "undefined external reference";
      begin
         if Errors = null then
            Trace (Me, "calling output wrapper when Errors callback not set");
            return;
         end if;

         if Index (S, Pattern) /= 0 then
            Undefined_Externals_Present := True;
         end if;

         Errors (S);
      end Catch_Undefined_Externals;

      Sources_Count : constant Source_File_Index :=
                        GPR.Sinput.Source_File_Last;
   begin
      if Self.Data.Env.IDE_Mode then
         GPR.Output.Set_Special_Output
           (Catch_Undefined_Externals'Unrestricted_Access);
      else
         GPR.Output.Set_Special_Output (GPR.Output.Output_Proc (Errors));
      end if;

      --  The views stored in the projects are no longer valid, we should make
      --  sure they are not called.

      declare
         C : Project_Htables.Cursor := Self.Data.Projects.First;
      begin
         while Has_Element (C) loop
            Element (C).Data.View := GPR.No_Project;
            Next (C);
         end loop;
      end;

      Reset_View (Self);
      GPR.Initialize (Self.Data.View);

      Opt.Follow_Links_For_Files := not Self.Data.Env.Trusted_Mode;
      Opt.Follow_Links_For_Dirs  := not Self.Data.Env.Trusted_Mode;

      begin
         Flags := Create_Flags
           (On_Error'Unrestricted_Access,
            Require_Sources     => False,
            Report_Missing_Dirs => Self.Data.Env.Report_Missing_Dirs);

         --  Make sure errors are reinitialized before load
         GPR.Err.Initialize;

         Override_Flags (Self.Data.Env.Env, Flags);

         Trace (Me, "Configuration file is '"
                & Self.Data.Env.Config_File.Display_Full_Name & "' autoconf="
                & Self.Data.Env.Autoconf'Img
                & " for target " & Self.Root_Project.Get_Target);

         --  Get_Target only returns a non-empty string when
         --  Set_Target_And_Runtime was called first; otherwise we depend on
         --  the project manager to extract target and runtime information
         --  from project attributes

         if Self.Root_Project.Data.Tree.Env.Forced_Runtime /= null then
            Set_Runtime_For
              (Get_String ("ada"),
               Self.Root_Project.Data.Tree.Env.Forced_Runtime.all);
         end if;

         Process_Project_And_Apply_Config
           (Main_Project        => View,
            User_Project_Node   => Self.Root_Project.Data.Node,
            Config_File_Name    => Self.Data.Env.Config_File.Display_Full_Name,
            Autoconf_Specified  => Self.Data.Env.Autoconf,
            Project_Tree        => Self.Data.View,
            Project_Node_Tree   => Self.Data.Tree,
            Packages_To_Check   => Self.Data.Env.Packages_To_Check,
            Target_Name                => Self.Root_Project.Get_Target,
            Allow_Automatic_Generation => Self.Data.Env.Autoconf,
            Automatically_Generated    => Automatically_Generated,
            Config_File_Path           => Config_File_Path,
            Env                        => Self.Data.Env.Env,
            Normalized_Hostname        => "",
            On_Load_Config             =>
              Add_GPS_Naming_Schemes_To_Config_File'Unrestricted_Access,
            On_New_Tree_Loaded         =>
              On_New_Tree_Loaded'Unrestricted_Access);

         Free (Config_File_Path);

         --  Should we reprocess with a different predefined path ?
         --  A similar test has already been done in Internal_Load, which
         --  ensures we are resolving the with clauses correctly and not
         --  looking for source file with the wrong path.
         --  But we need to do this test again, in case the user has changed
         --  the scenario variables and they influence which gnatls command is
         --  run. Unfortunately, this mean we might have spent time looking
         --  for incorrect sources above.
         --  ??? It might be simpler to hide the Recompute_View altogether and
         --  force users to reload the project systematically (this would not
         --  change performance most likely)

         Trace (Me, "Checking whether the gnatls attribute has changed");
         if View /= GPR.No_Project
            and then Set_Path_From_Gnatls_Attribute (View, Self, Errors)
         then
            Trace (Me, "recompute view a second time with proper path");
            Reset_View (Self);
            GPR.Initialize (Self.Data.View);

            Process_Project_And_Apply_Config
              (Main_Project        => View,
               User_Project_Node   => Self.Root_Project.Data.Node,
               Config_File_Name    =>
                 Self.Data.Env.Config_File.Display_Full_Name,
               Autoconf_Specified  => Self.Data.Env.Autoconf,
               Project_Tree        => Self.Data.View,
               Project_Node_Tree   => Self.Data.Tree,
               Packages_To_Check   => Self.Data.Env.Packages_To_Check,
               Allow_Automatic_Generation => Self.Data.Env.Autoconf,
               Automatically_Generated    => Automatically_Generated,
               Config_File_Path           => Config_File_Path,
               Env                        => Self.Data.Env.Env,
               Normalized_Hostname        => "",
               On_Load_Config             =>
                 Add_GPS_Naming_Schemes_To_Config_File'Unrestricted_Access,
               On_New_Tree_Loaded         =>
                 On_New_Tree_Loaded'Unrestricted_Access);

            Free (Config_File_Path);
         end if;

         Override_Flags (Self.Data.Env.Env, Create_Flags (null));

      exception
         when E : Invalid_Config =>
            Trace (Me, Exception_Message (E));  --  not the exception itself
            if Errors /= null then
               Errors (Exception_Message (E));
            end if;
            Override_Flags (Self.Data.Env.Env, Create_Flags (null));
            --  Error message was already reported via GPR.Err
            null;
      end;

      --  Backward compatibility: load the project even if there was a fatal
      --  error. However, the view might be partial...
      --    if View = null then
      --       raise Invalid_Project;
      --    end if;

      Trace (Me, "View has been recomputed");

      --  Now that we have the view, we can create the project instances

      if View = GPR.No_Project then
         --  There was an error, but we still want to manipulate that project
         Self.Data.Root.Data.View :=
           Get_View (Self.Data.View,
                     Path => GPR.Tree.Path_Name_Of
                       (Self.Data.Root.Data.Node, Self.Data.Tree));
      else
         Self.Data.Root.Data.View := View;
      end if;

      Create_Project_Instances (Self, Self, With_View => True);

      --  To get scenario variables from aggregated projects we first need
      --  all to fully parse all project trees and create instances of all
      --  projects.

      Compute_Scenario_Variables (Self.Data, Errors => Errors);

      Parse_Source_Files (Self);
      Initialize_Source_Records;

      --  If the timestamp have not been computed yet (ie we are loading a new
      --  project), do it now.
      --  We cannot simply use Clock here, since this returns local time,
      --  and the file timestamps will be returned in GMT, therefore we
      --  won't be able to compare.

      if Self.Data.Timestamp = GNATCOLL.Utils.No_Time
        and then Self.Data.Status = From_File
      then
         Iter := Start (Self.Root_Project);

         while Current (Iter) /= No_Project loop
            Timestamp := File_Time_Stamp (Project_Path (Current (Iter)));

            if Timestamp > Self.Data.Timestamp then
               Self.Data.Timestamp := Timestamp;
            end if;

            Next (Iter);
         end loop;
      end if;

      --  ??? Should not be needed since all errors are reported through the
      --  callback already. This avoids duplicate error messages in the console

      GPR.Err.Finalize;
      GPR.Output.Cancel_Special_Output;

      if Undefined_Externals_Present then
         Errors
           ("Some externals are undefined, project may be loaded incompletely"
            & ASCII.LF);
         Errors
           ("Set values of corresponding externals and reload the project"
            & ASCII.LF);
      end if;

      GPR.Sinput.Source_File_Trim (Sources_Count);

      --  Save the config file that was used to disk, if needed. This will
      --  be used when spawning other project-aware tools, since it might
      --  include extra naming schemes coming from calls to
      --  Register_Default_Language_Extension.

      if Self.Data.Env.Save_Config_File /= null
         and then Actual_Config_File /= Empty_Project_Node
         and then Self.Status = From_File
      then
         declare
            F : Ada.Text_IO.File_Type;
            type File_Pretty_Printer is new Pretty_Printer with null record;
            overriding procedure Put
               (Self : in out File_Pretty_Printer; C : Character);

            overriding procedure Put
               (Self : in out File_Pretty_Printer; C : Character)
            is
               pragma Unreferenced (Self);
            begin
               Put (F, C);
            end Put;

            P : File_Pretty_Printer;
            Gpsauto : Virtual_File;
            Dir : Virtual_File := Self.Root_Project.Object_Dir;

         begin
            if Dir = GNATCOLL.VFS.No_File then
               Dir := Create (Self.Root_Project.Project_Path.Dir_Name);
            end if;

            Gpsauto := Create_From_Dir
               (Dir => Dir,
                Base_Name => +Self.Data.Env.Save_Config_File.all);

            Trace (Me, "Saving config file to " & Gpsauto.Display_Full_Name);
            Ada.Text_IO.Create (F, Out_File, Gpsauto.Display_Full_Name);
            Put (Self    => P,
                 Project => Actual_Config_File,
                 In_Tree => Actual_Config_File_Tree);
            Close (F);
         exception
            when Ada.Text_IO.Name_Error
               | Ada.Text_IO.Use_Error =>
               Trace (Me, "Could not save config file");
         end;
      end if;

   exception
      --  We can get an unexpected exception (actually Directory_Error) if the
      --  project file's path is invalid, for instance because it was
      --  modified by the user.

      when Invalid_Project =>
         Trace (Me, "Could not compute project view");
         GPR.Err.Finalize;
         GPR.Output.Cancel_Special_Output;
         raise;

      when E : others =>
         Trace (Me, E);
         GPR.Err.Finalize;
         GPR.Output.Cancel_Special_Output;
   end Recompute_View;

   ------------------------
   -- Instance_From_Node --
   ------------------------

   function Instance_From_Node
     (Self         : Project_Tree'Class;
      Tree_For_Map : Project_Tree'Class;
      Node         : Project_Node_Id) return Project_Type
   is
      Path : constant Virtual_File :=
        Create (+Get_String (GPR.Tree.Path_Name_Of (Node, Self.Data.Tree)));
      Data : Project_Data_Access;
      P    : Project_Type;
      C    : constant Project_Htables.Cursor :=
        Tree_For_Map.Data.Projects.Find (Path);
   begin
      if not Has_Element (C) then
         Data              := Tree_For_Map.Data_Factory;
         Data.Tree         := Self.Data;
         Data.Tree_For_Map := Tree_For_Map.Data;
         Data.Node         := Node;
         P := Project_Type'(Ada.Finalization.Controlled with Data => Data);
         Tree_For_Map.Data.Projects.Include (Path, P);
         return P;
      else
         return Element (C);
      end if;
   end Instance_From_Node;

   ------------------------------
   -- Create_Project_Instances --
   ------------------------------

   procedure Create_Project_Instances
     (Self      : Project_Tree'Class;
      Tree_For_Map : Project_Tree'Class;
      With_View : Boolean)
   is
      procedure Do_Project
        (Proj : Project_Id;
         Tree : Project_Tree_Ref;
         S    : in out Integer);

      procedure Do_Project2 (T : GPR.Project_Node_Tree_Ref;
                             P : Project_Node_Id);

      ----------------
      -- Do_Project --
      ----------------

      procedure Do_Project
        (Proj : Project_Id;
         Tree : Project_Tree_Ref;
         S    : in out Integer)
      is
         pragma Unreferenced (S); --  , Tree);
         Iter : Project_Htables.Cursor;
         P    : Project_Type;
         Path : Virtual_File;

      begin
         if not Proj.Virtual then
            Path := Create (+Get_String (Proj.Path.Display_Name));
            Iter := Tree_For_Map.Data.Projects.Find (Path);

            if Has_Element (Iter) then
               P := Element (Iter);
               Reset_View (P.Data.all);

               --  For a given project, it does not matter much whether we are
               --  seeing the view from one aggregated project or another. But
               --  we must ensure that the project_id matches the view from the
               --  tree, otherwise the project will not be found by the prj*
               --  packages.
               if P.Data.Tree.View = null
                  or else P.Data.Tree.View = Tree
               then
                  P.Data.View := Proj;
                  P.Data.Tree.View := Tree;  --  must match Proj
               end if;
            elsif Active (Me) then
               Assert (Me, False,
                       "Create_Project_Instances must be called"
                       & " to create project_type for "
                       & Path.Display_Full_Name);
            end if;
         end if;
      end Do_Project;

      -----------------
      -- Do_Project2 --
      -----------------

      procedure Do_Project2 (T : GPR.Project_Node_Tree_Ref;
                             P : Project_Node_Id) is
         pragma Unreferenced (T);
         Proj : Project_Type;
      begin
         Proj := Instance_From_Node (Self, Tree_For_Map, Node => P);
         Reset_View (Proj.Data.all);
      end Do_Project2;

      procedure For_All_Projects is new For_Every_Project_Imported
        (Integer, Do_Project);

      S : Integer := 0;
   begin
      if With_View then
         Assert (Me, Self.Data.Root.Data.View /= null,
                 "Create_Project_Instances: Project not parsed");
         For_All_Projects
           (Self.Data.Root.Data.View,
            Self.Data.View,
            S,
            Include_Aggregated => True);

      else
         For_Each_Project_Node
           (Self.Data.Tree, Self.Data.Root.Data.Node,
            Do_Project2'Unrestricted_Access);
      end if;
   end Create_Project_Instances;

   ------------------------
   -- Load_Empty_Project --
   ------------------------

   procedure Load_Empty_Project
     (Self : in out Project_Tree;
      Env  : Project_Environment_Access := null;
      Name : String := "empty";
      Recompute_View : Boolean := True)
   is
      D : constant Filesystem_String :=
            Name_As_Directory (Get_Current_Dir)
            & (+Name) & Project_File_Extension;
      Node : Project_Node_Id;
   begin
      Trace (Me, "Loading empty project");

      Project_Tree'Class (Self).Unload;

      Reset (Self, Env);

      Node := GPR.Tree.Create_Project
        (In_Tree        => Self.Data.Tree,
         Name           => Get_String (Name),
         Full_Path      => Path_Name_Type (Get_String (+D)),
         Is_Config_File => False);

      Self.Data.Root := Self.Instance_From_Node (Self, Node);
      Self.Set_Status (Empty);

      --  No language known for empty project

      Self.Data.Root.Set_Attribute (Languages_Attribute, (1 .. 0 => null));

      Self.Data.Root.Data.Modified := False;

      Create_Project_Instances (Self, Self, With_View => False);

      if Recompute_View then
         Project_Tree'Class (Self).Recompute_View;
      end if;
   end Load_Empty_Project;

   ---------------------------
   -- Load_Implicit_Project --
   ---------------------------

   procedure Load_Implicit_Project
     (Self : in out Project_Tree;
      Env  : Project_Environment_Access := null;
      Recompute_View : Boolean := True)
   is
      Project_File  : Virtual_File;
      Gprbuild_Path : Filesystem_String_Access;
      Project       : Project_Node_Id;

      Implicit_Project_File_Path : constant String :=
        "share" &
        Directory_Separator &
        "gpr" &
        Directory_Separator &
        '_' &
        Default_Project_File_Name;
   begin
      Trace (Me, "Loading implicit project ");

      Project_Tree'Class (Self).Unload;

      Reset (Self, Env);

      Gprbuild_Path := Locate_Exec_On_Path ("gprbuild");
      if Gprbuild_Path = null then
         Trace (Me, "Gprbuild not found on path");
         return;
      end if;
      Project_File := Get_Parent (Create (Dir_Name (Gprbuild_Path.all)));
      Project_File := Join (Project_File, +Implicit_Project_File_Path);
      Free (Gprbuild_Path);

      if not Project_File.Is_Regular_File then
         Trace (Me, "_default.gpr not found in expected location");
         return;
      end if;

      Trace (Me, "Implicit project found " & Project_File.Display_Full_Name);

      Internal_Load
        (Self, Project_File, null,
         Report_Syntax_Errors => True,   --  _default.gpr is safe
         Project              => Project,
         Packages_To_Check    => No_Packs,
         Recompute_View       => Recompute_View,
         Implicit_Project     => True);

      if Project = Empty_Project_Node then
         Trace (Me, "Cannot load implicit project");
         return;
      end if;
   end Load_Implicit_Project;

   ------------------------
   -- Parse_Source_Files --
   ------------------------

   procedure Parse_Source_Files (Self : in out Project_Tree) is
      Block_Me : constant Block_Trace_Handle := Create (Me);

      procedure Register_Directory (Directory : Filesystem_String);
      --  Register Directory as belonging to Project.
      --  The parent directories are also registered.

      ------------------------
      -- Register_Directory --
      ------------------------

      procedure Register_Directory (Directory : Filesystem_String) is
         Dir  : constant Filesystem_String := Name_As_Directory (Directory);
         Last : Integer := Dir'Last - 1;
         Curs : Directory_Statuses.Cursor;
      begin
         Self.Data.Directories.Include (Dir, Direct);

         loop
            while Last >= Dir'First
              and then Dir (Last) /= Directory_Separator
              and then Dir (Last) /= '/'
            loop
               Last := Last - 1;
            end loop;

            Last := Last - 1;

            exit when Last <= Dir'First;

            --  Register the name with a trailing directory separator

            Curs := Self.Data.Directories.Find (Dir (Dir'First .. Last + 1));
            if not Has_Element (Curs) or else Element (Curs) /= Direct then
               Self.Data.Directories.Include
                 (Dir (Dir'First .. Last + 1), As_Parent);
            end if;
         end loop;
      end Register_Directory;

      use Virtual_File_List;

      Gnatls           : constant String :=
                           Self.Root_Project.Attribute_Value
                             (Gnatlist_Attribute);
      Iter             : Project_Iterator;
      Sources          : String_List_Id;
      P                : Project_Type;
      Source_Iter      : Source_Iterator;
      Source           : Source_Id;
      Source_File_List : Virtual_File_List.List;
      Tree_For_Map     : constant Project_Tree_Data_Access :=
                           Self.Data.Root.Data.Tree_For_Map;
   begin
      Tree_For_Map.Objects_Basename.Clear;

      Iter := Self.Root_Project.Start (Recursive => True);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         declare
            Ls : constant String := P.Attribute_Value (Gnatlist_Attribute);
         begin
            if Ls /= "" and then Ls /= Gnatls then
               --  We do not want to mark the project as incomplete for this
               --  warning, so we do not need to pass an actual Error_Handler
               GPR.Err.Error_Msg
                 (Flags => Create_Flags (null),
                  Msg   =>
                   "?the project attribute IDE.gnatlist doesn't have"
                  & " the same value as in the root project."
                  & " The value """ & Gnatls & """ will be used",
                  Project => Get_View (P));
            end if;
         end;

         --  Reset the list of source files for this project. We must not
         --  Free it, since it is now stored in the previous project's instance

         Source_File_List := Virtual_File_List.Empty_List;

         --  Add the directories

         Sources := Get_View (P).Source_Dirs;
         while Sources /= Nil_String loop
            Register_Directory
              (+Get_String (String_Elements (Self.Data)(Sources).Value));
            Sources := String_Elements (Self.Data)(Sources).Next;
         end loop;

         Register_Directory (+Get_String (Get_View (P).Object_Directory.Name));
         Register_Directory (+Get_String (Get_View (P).Exec_Directory.Name));

         --  Add the sources that are already in the project.
         --  Convert the names to UTF8 for proper handling in GPS

         Source_Iter := For_Each_Source
           (P.Data.Tree.View, Get_View (P), Locally_Removed => False);
         loop
            Source := Element (Source_Iter);
            exit when Source = No_Source;

            if Source.Replaced_By /= No_Source then
               --  In case of extending project inheriting package Naming of
               --  the extended one and source dirs of both projects containing
               --  same naming exception source, we will get a duplicate base
               --  name outside of aggregate project here which is not allowed.
               --  The replaced source needs to be ignored.
               goto Next_Source;
            end if;

            --  Get the absolute path name for this source

            Get_Name_String (Source.Path.Display_Name);

            declare
               File : constant Virtual_File :=
                        Create (+Name_Buffer (1 .. Name_Len));
            begin
               if Self.Data.Root.Is_Aggregate_Project then
                  --  If we have duplicates, create lists
                  Include_File
                    (Tree_For_Map.Sources,
                     Base_Name (File),
                     (P, File, Source.Language.Name, Source, null));
               else
                  --  No point in all the checks for regular project.

                  Tree_For_Map.Sources.Include
                    (Base_Name (File),
                     (P, File, Source.Language.Name, Source, null));
               end if;

               if Source.Object /= GPR.No_File
                 and then Source.Language /= null
                   -- and then Source.Language.Config.Object_File_Suffix /=
                   -- Name_Op_Subtract  ????
                 and then Get_String
                   (Source.Language.Config.Object_File_Suffix) /= "-"
               then
                  declare
                     Obj_Suffix_Attr : constant String :=
                       Attribute_Value
                         (Self.Data.Root,
                          "compiler#object_file_suffix",
                          "ada");

                     Base : constant Filesystem_String :=
                              Base_Name
                                (Filesystem_String
                                   (Get_String (Source.Object)),
                                 +(if Obj_Suffix_Attr /= ""
                                   then Obj_Suffix_Attr
                                   else ".o"));
                     Base_Last : Natural := Base'Last;
                  begin
                     --  In GPS, users might define ada-based languages
                     --  when they have local variations. In this case,
                     --  they are likely to define the object suffix as
                     --  ".ali", which we need to ignore as well.

                     if Utils.Ends_With (String (Base), ".ali") then
                        Base_Last := Base_Last - 4;
                     end if;

                     --  We know the actual object file will be in either
                     --  P or one of its extending projects. We can't
                     --  compute this information now though, because the
                     --  sources might not have been compiled. So the final
                     --  computation is done directly in Library_Files.

                     if Source.Index = 0 then
                        --  ??? What if we have a non-aggregate root, that
                        --  imports a library aggregate project ?
                        --  if Is_Aggregate_Project (Self.Data.Root) then
                        Include_File
                          (Tree_For_Map.Objects_Basename,
                           Base (Base'First .. Base_Last),
                           (P, File, Source.Language.Name, Source,
                            null));
                        --  else
                        --     --  No point in all the checks for regular
                        --     --  project.
                        --
                        --     Tree_For_Map.Objects_Basename.Include
                        --       (Base (Base'First .. Base_Last),
                        --          (P, File, Source.Language.Name, Source,
                        --           null));
                        --  end if;

                     else
                        --  if Is_Aggregate_Project (Self.Data.Root) then
                        Include_File
                          (Tree_For_Map.Objects_Basename,
                           Base (Base'First .. Base_Last) & "~"
                           & (+Image
                             (Integer (Source.Index),
                                  Min_Width => 0)),
                           (P, File, Source.Language.Name, Source,
                            null));
                        --  else
                        --     --  No point in all the checks for regular
                        --     --  project.
                        --
                        --     Tree_For_Map.Objects_Basename.Include
                        --       (Base (Base'First .. Base_Last) & "~"
                        --        & (+Image
                        --          (Integer (Source.Index),
                        --               Min_Width => 0)),
                        --        (P, File, Source.Language.Name, Source,
                        --         null));
                        --  end if;
                     end if;
                  end;
               end if;

               --  The project manager duplicates files that contain several
               --  units. Only add them once in the project sources
               --  (and thus only when the Index is 0 (single unit) or 1
               --  (first of multiple units).
               --  For source-based languages, we allow duplicate sources

               if Source.Unit = null or else Source.Index <= 1 then
                  Prepend (Source_File_List, File);
               end if;
            end;

            <<Next_Source>>
            Next (Source_Iter);
         end loop;

         --  Register the sources in our own caches

         declare
            Count   : constant Ada.Containers.Count_Type :=
                        Virtual_File_List.Length (Source_File_List);
            Files   : constant File_Array_Access :=
                        new File_Array (1 .. Natural (Count));
            Current : Virtual_File_List.Cursor := First (Source_File_List);
            J       : Natural := Files'First;
         begin
            while Has_Element (Current) loop
               --  ??? Create new virtual files to work around compiler bug.
               --  The ideal would have been to write:
               --     Files (J) := Element (Current)
               --  in order to avoid memory reallocations.
               Files (J) := Create (Element (Current).Full_Name);
               Next (Current);
               J := J + 1;
            end loop;

            Unchecked_Free (P.Data.Files);
            P.Data.Files := Files;

            if P.Data.Base_Name_To_Full_Path = null then
               P.Data.Base_Name_To_Full_Path := new Basename_To_Info_Cache.Map;
            else
               P.Data.Base_Name_To_Full_Path.Clear;
            end if;

            for F of P.Data.Files.all loop
               P.Data.Base_Name_To_Full_Path.Include (String (F.Base_Name), F);
            end loop;
         end;

         Next (Iter);
      end loop;
   end Parse_Source_Files;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Project_Tree) is
      Iter : Project_Htables.Cursor;
      Data : Project_Data_Access;

   begin
      if Self.Data = null then
         return;
      end if;

      Iter := Self.Data.Projects.First;

      --  Since we are going to free the tree, removing any reference to it in
      --  the projects that the user might keep around

      while Has_Element (Iter) loop
         Data := Element (Iter).Data;
         if Data.Tree.Tree /= Self.Data.Tree then
            Free (Data.Tree.Tree);
         end if;
         Data.Tree := null;
         Reset_View (Data.all);
         Data.Node := Empty_Project_Node;
         Next (Iter);
      end loop;

      if Self.Data.View /= null then
         Reset (Self.Data.View);
      end if;

      Self.Data.Root := No_Project;

      GPR.Tree_Private_Part.Projects_Htable.Reset
        (Self.Data.Tree.Projects_HT);
      Sinput.Clear_Source_File_Table;
      Sinput.Reset_First;

      --  Reset the scenario variables.
      --  The issue is that a given variable might currently have a value, and
      --  then be used in another project where that value is now illegal.
      --  Do not reset if we have an empty project, since otherwise we lose the
      --  values set from the command line
      --  ??? Don't reset after all, this is too tricky to get right, and might
      --  be plain wrong in fact.

--        if Self.Data.Status /= Empty then
--           GPR.Ext.Reset (Self.Data.Tree);
--        end if;

      Reset_View (Self);

      --  Free all projects. This will decrease the refcounting for their data
      --  and possibly free the memory

      Self.Data.Projects.Clear;

      --  Do not reset the tree node, since it also contains the environment
      --  variables, which we want to preserve in case the user has changed
      --  them before loading the project.

      Free (Self.Data.View);
   end Unload;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment
     (Self : Project_Type) return Project_Environment_Access is
   begin
      if Self = No_Project
        or Self.Data.Tree = null
      then
         return null;
      else
         return Self.Data.Tree.Env;
      end if;
   end Get_Environment;

   --------------------------
   -- Is_Aggregate_Library --
   --------------------------

   function Is_Aggregate_Library (Self : Project_Type) return Boolean is
   begin
      return Project_Qualifier_Of
        (Self.Data.Node, Self.Data.Tree.Tree) = GPR.Aggregate_Library;
   end Is_Aggregate_Library;

   --------------------------
   -- Is_Aggregate_Project --
   --------------------------

   function Is_Aggregate_Project (Self : Project_Type) return Boolean is
   begin
      if Self = No_Project
        or else Self.Data = null
        or else Self.Data.Tree = null
      then
         return False;
      else
         return Project_Qualifier_Of
           (Self.Data.Node, Self.Data.Tree.Tree) in GPR.Aggregate_Project;
      end if;
   end Is_Aggregate_Project;

   -------------------------
   -- Is_Abstract_Project --
   -------------------------

   function Is_Abstract_Project (Self : Project_Type) return Boolean is
   begin
      return Project_Qualifier_Of
        (Self.Data.Node, Self.Data.Tree.Tree) = Abstract_Project;
   end Is_Abstract_Project;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable (Project : Project_Type) return Boolean is
      Att : constant Attribute_Pkg_String :=
        Build ("IDE", "Read_Only");
   begin
      return (Project.Project_Path.Is_Writable
              or else not Project.Project_Path.Is_Regular_File)
        and then not Project.Data.Uses_Variables
        and then not Project.Data.Tree.Root.Is_Aggregate_Project
        and then Project.Data.View_Is_Complete
        and then (not Project.Has_Attribute (Att)
                  or else To_Lower (Project.Attribute_Value (Att)) /= "true");
   end Is_Editable;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
--        GPR.Finalize;
--        Atree.Atree_Private_Part.Nodes.Free;
   end Finalize;

   ---------
   -- Put --
   ---------

   procedure Put (Self : in out Pretty_Printer; C : Character) is
      pragma Unreferenced (Self);
   begin
      Ada.Text_IO.Put (C);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Self : in out Pretty_Printer; S : String) is
   begin
      for C in S'Range loop
         Put (Pretty_Printer'Class (Self), S (C));
      end loop;
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Self : in out Pretty_Printer) is
   begin
      Put (Pretty_Printer'Class (Self), ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put
     (Self                 : in out Pretty_Printer;
      Project              : Project_Type'Class;
      Increment            : Positive              := 3;
      Eliminate_Empty_Case_Constructions : Boolean := False)
   is
   begin
      Put
        (Self      => Self,
         Project   => Project.Data.Node,
         In_Tree   => Project.Data.Tree.Tree,
         Id        => Project.Data.View,
         Increment => Increment,
         Eliminate_Empty_Case_Constructions =>
            Eliminate_Empty_Case_Constructions);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
      (Self        : in out Pretty_Printer'Class;
       Project     : Project_Node_Id;
       In_Tree     : GPR.Project_Node_Tree_Ref;
       Id          : Project_Id := GPR.No_Project;
       Increment   : Positive := 3;
       Eliminate_Empty_Case_Constructions : Boolean := False)
   is
      procedure W_Char (C : Character);
      procedure W_Eol;
      procedure W_Str  (S : String);

      ------------
      -- W_Char --
      ------------

      procedure W_Char (C : Character) is
      begin
         Put (Self, C);
      end W_Char;

      -----------
      -- W_Eol --
      -----------

      procedure W_Eol is
      begin
         New_Line (Self);
      end W_Eol;

      -----------
      -- W_Str --
      -----------

      procedure W_Str (S : String) is
      begin
         Put (Self, S);
      end W_Str;

   begin
      GPR.PP.Pretty_Print
        (Project                            => Project,
         In_Tree                            => In_Tree,
         Increment                          => Increment,
         Eliminate_Empty_Case_Constructions =>
           Eliminate_Empty_Case_Constructions,
         Minimize_Empty_Lines               => False,
         W_Char                             => W_Char'Unrestricted_Access,
         W_Eol                              => W_Eol'Unrestricted_Access,
         W_Str                              => W_Str'Unrestricted_Access,
         Backward_Compatibility             => False,
         Id                                 => Id);
   end Put;

   ----------
   -- Node --
   ----------

   function Node
     (Project : Project_Type'Class) return GPR.Project_Node_Id is
   begin
      return Project.Data.Node;
   end Node;

   ----------
   -- Tree --
   ----------

   function Tree
     (Data : Project_Tree_Data_Access) return GPR.Project_Node_Tree_Ref is
   begin
      return Data.Tree;
   end Tree;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_List;
      Values    : GNAT.Strings.String_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Prepend   : Boolean := False) is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GNATCOLL.Projects.Normalize.Set_Attribute
        (Self.Data.Tree, Self, Attribute, Values, Scenario, Index, Prepend);
   end Set_Attribute;

   procedure Set_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_String;
      Value     : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      At_Index  : Natural := 0) is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GNATCOLL.Projects.Normalize.Set_Attribute
        (Self.Data.Tree, Self, Attribute, Value, Scenario, Index,
         At_Index);
   end Set_Attribute;

   ----------------------
   -- Delete_Attribute --
   ----------------------

   procedure Delete_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "") is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GNATCOLL.Projects.Normalize.Delete_Attribute
        (Self.Data.Tree, Self, String (Attribute), Scenario, Index);
   end Delete_Attribute;

   procedure Delete_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "") is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GNATCOLL.Projects.Normalize.Delete_Attribute
        (Self.Data.Tree, Self, String (Attribute), Scenario, Index);
   end Delete_Attribute;

   ---------------------
   -- Rename_And_Move --
   ---------------------

   procedure Rename_And_Move
     (Self      : Project_Type;
      New_Name  : String;
      Directory : GNATCOLL.VFS.Virtual_File;
      Errors    : Error_Report := null)
   is
      Old_Path : constant Virtual_File := Self.Project_Path;

   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GNATCOLL.Projects.Normalize.Rename_And_Move
        (Self.Data.Tree, Self, New_Name, Directory, Errors);

      Self.Data.Tree.Projects.Delete (Old_Path);

      Self.Data.Tree.Projects.Include (Self.Project_Path, Self);

      if Self.Data.View /= GPR.No_Project then
         Self.Data.View.Display_Name := Get_String (New_Name);
      end if;

      --  This is no longer the default project, since it was
      --  renamed. Otherwise, Project_Path would still return "" when saving
      --  the default project.

      Trace (Me, "Set project status to From_File");
      Self.Data.Tree.Status := From_File;

      Reset_All_Caches (Self.Data.Tree);
   end Rename_And_Move;

   ----------------------------
   -- Register_New_Attribute --
   ----------------------------

   function Register_New_Attribute
     (Name                 : String;
      Pkg                  : String;
      Is_List              : Boolean := False;
      Indexed              : Boolean := False;
      Case_Sensitive_Index : Boolean := False) return String
   is
      Lower_Pkg : constant String := To_Lower (Pkg);
      Pkg_Id    : Package_Node_Id := Empty_Package;
      Attr_Id   : Attribute_Node_Id;
      Attr_Kind : Defined_Attribute_Kind;
      Var_Kind  : Defined_Variable_Kind;
   begin
      --  Need to make sure the predefined packages are already declared, or
      --  the new one will be discarded.

      GPR.Attr.Initialize;

      if Lower_Pkg /= "" then
         Pkg_Id := Package_Node_Id_Of (Get_String (Lower_Pkg));
         if Pkg_Id = Empty_Package or else Pkg_Id = Unknown_Package then
            Trace (Me, "Register_New_Package (" & Lower_Pkg & ")");
            Register_New_Package (Name  => Lower_Pkg, Id => Pkg_Id);
            if Pkg_Id = Empty_Package or else Pkg_Id = Unknown_Package then
               Trace (Me, "Error registering new package");
            end if;
         end if;
      end if;

      if Pkg_Id = Empty_Package then
         Attr_Id := Attribute_Node_Id_Of
           (Name        => Get_String (Name),
            Starting_At => GPR.Attr.Attribute_First);
      else
         Attr_Id := Attribute_Node_Id_Of
           (Name        => Get_String (Name),
            Starting_At => First_Attribute_Of (Pkg_Id));
      end if;

      if Is_List then
         Var_Kind := GPR.List;
      else
         Var_Kind := GPR.Single;
      end if;

      if Indexed then
         if Case_Sensitive_Index then
            Attr_Kind := GPR.Attr.Associative_Array;
         else
            Attr_Kind := GPR.Attr.Case_Insensitive_Associative_Array;
         end if;

         --  Priority is given to the registered type
         if Attr_Id /= Empty_Attribute then
            Attr_Kind := Attribute_Kind_Of (Attr_Id);
            if Attr_Kind = Attribute_Kind'(Single) then
               Attr_Kind := GPR.Attr.Associative_Array;
            end if;
         end if;
      else
         Attr_Kind := Attribute_Kind'(Single);
      end if;

      if Attr_Id = Empty_Attribute then
         if Lower_Pkg = "" then
            return "Project attributes cannot be added at the top level of"
              & " project files, only in packages";

         else
            if Active (Me) then
               Trace (Me, "Register_New_Attribute (" & Name
                      & ", " & Lower_Pkg & ", " & Attr_Kind'Img & ", "
                      & Var_Kind'Img & ")");
            end if;

            Register_New_Attribute
              (Name               => Name,
               In_Package         => Pkg_Id,
               Attr_Kind          => Attr_Kind,
               Var_Kind           => Var_Kind,
               Index_Is_File_Name => False,
               Opt_Index          => False);
         end if;

      else
         if Attribute_Kind_Of (Attr_Id) /= Attr_Kind
           or else Variable_Kind_Of (Attr_Id) /= Var_Kind
         then
            return Name
              & ": attributes was already defined but with a"
              & " different type";
         end if;
      end if;

      return "";
   end Register_New_Attribute;

   ----------------------------------
   -- Register_Specific_Attributes --
   ----------------------------------

   procedure Register_Specific_Attributes is
   begin
      if Specific_Attributes_Registered then
         --  Already registered during previous loads, nothing to do.
         return;
      end if;

      if not Attribute_Registered ("Artifacts_Dir", "IDE") then
         declare
            S : constant String :=
              Register_New_Attribute ("Artifacts_Dir", "IDE");
         begin
            if S /= "" then
               Trace (Me, "Cannot register attribute IDE'Artefact_Dir: " & S);
            end if;
         end;
      end if;

      if not Attribute_Registered ("Read_Only", "IDE") then
         declare
            S : constant String :=
              Register_New_Attribute ("Read_Only", "IDE");
         begin
            if S /= "" then
               Trace (Me, "Cannot register attribute IDE'Artefact_Dir: " & S);
            end if;
         end;
      end if;

      --  If it didn't work the first time it won't work at all, no use trying
      --  again.
      Specific_Attributes_Registered := True;
   end Register_Specific_Attributes;

   ----------
   -- Save --
   ----------

   function Save
     (Project : Project_Type;
      Force   : Boolean := False;
      Errors  : Error_Report := null) return Boolean
   is
      File : Ada.Text_IO.File_Type;

      type File_Pretty_Printer is new Pretty_Printer with null record;
      overriding procedure Put
        (Self : in out File_Pretty_Printer; C : Character);
      overriding procedure Put (Self : in out File_Pretty_Printer; S : String);

      ---------
      -- Put --
      ---------

      overriding procedure Put
        (Self : in out File_Pretty_Printer; C : Character)
      is
         pragma Unreferenced (Self);
      begin
         Put (File, C);
      end Put;

      overriding procedure Put
        (Self : in out File_Pretty_Printer; S : String)
      is
         pragma Unreferenced (Self);
      begin
         Put (File, S);
      end Put;

      PP : File_Pretty_Printer;

   begin
      if not Project.Is_Editable then
         raise Project_Not_Editable;
      end if;

      if not Is_Regular_File (Project.Project_Path)
        or else Project.Data.Modified
        or else Force
      then
         if Is_Regular_File (Project_Path (Project))
           and then not Is_Writable (Project_Path (Project))
         then
            if Errors /= null then
               Errors
                 ("The file " & Display_Full_Name (Project_Path (Project))
                  & " is not writable. Project not saved");
            end if;
            Trace (Me, "Project file not writable: "
                   & Project_Path (Project).Display_Full_Name);
            return False;
         end if;

         declare
            Filename : constant Virtual_File := Project_Path (Project);
            Dirname  : Virtual_File renames Dir (Filename);
         begin
            Trace (Me, "Save_Project: Creating new file "
                   & Filename.Display_Full_Name);

            begin
               Ada.Directories.Create_Path (Dirname.Display_Full_Name);
            exception
               when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
                  Trace (Me, "Couldn't create directory " &
                         Dirname.Display_Full_Name);

                  if Errors /= null then
                     Errors
                       ("Couldn't create directory " &
                        Dirname.Display_Full_Name);
                  end if;

                  return False;
            end;

            Normalize_Cases (Project.Data.Tree.Tree, Project);

            Create (File, Mode => Out_File, Name => +Full_Name (Filename));
            PP.Put (Project => Project);
            Close (File);

            Project.Data.Modified := False;

            Trace (Me, "Set project status to From_File");
            Project.Data.Tree.Status := From_File;

            return True;

         exception
            when Ada.Text_IO.Name_Error =>
               Trace (Me, "Couldn't create " & Filename.Display_Full_Name);

               if Errors /= null then
                  Errors
                    ("Couldn't create file " & Filename.Display_Full_Name);
               end if;

               return False;
         end;
      end if;
      return False;
   end Save;

   --------------
   -- Modified --
   --------------

   function Modified
     (Project   : Project_Type;
      Recursive : Boolean := False) return Boolean
   is
      Iter : Inner_Project_Iterator := Start (Project, Recursive);
      P    : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = GNATCOLL.Projects.No_Project;

         if P.Data.Modified then
            return True;
         end if;
         Next (Iter);
      end loop;

      return False;
   end Modified;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified (Project : Project_Type; Modified : Boolean) is
   begin
      Project.Data.Modified := Modified;
   end Set_Modified;

   -----------------------------
   -- Remove_Imported_Project --
   -----------------------------

   procedure Remove_Imported_Project
     (Project          : Project_Type;
      Imported_Project : Project_Type)
   is
      Tree        : constant GPR.Project_Node_Tree_Ref :=
                      Project.Data.Tree.Tree;
      With_Clause : Project_Node_Id :=
                      First_With_Clause_Of (Project.Node, Tree);
      Next        : Project_Node_Id;

      Iter   : Project_Iterator;
      Remove : Boolean := True;

      Basename  : constant Filesystem_String :=
        Base_Name
          (Imported_Project.Project_Path.Full_Name,
           Project_File_Extension);

      Dep_ID : constant Name_Id := Get_String (+Basename);

      Tree_Node : constant GPR.Project_Node_Tree_Ref := Project.Data.Tree.Tree;
   begin
      if not Project.Is_Editable then
         raise Project_Not_Editable;
      end if;

      if With_Clause /= Empty_Project_Node
        and then GPR.Tree.Name_Of (With_Clause, Tree) =
        GPR.Tree.Name_Of (Imported_Project.Node, Tree)
      then
         Set_First_With_Clause_Of
           (Project.Node, Tree, Next_With_Clause_Of (With_Clause, Tree));
      else
         loop
            Next := Next_With_Clause_Of (With_Clause, Tree);
            exit when Next = Empty_Project_Node;

            if GPR.Tree.Name_Of (Next, Tree) =
              GPR.Tree.Name_Of (Imported_Project.Node, Tree)
            then
               Set_Next_With_Clause_Of
                 (With_Clause, Tree, Next_With_Clause_Of (Next, Tree));
            end if;

            With_Clause := Next;
         end loop;
      end if;

      Project.Data.Modified := True;

      --  Need to reset all the caches, since the caches contain the indirect
      --  dependencies as well.
      Reset_All_Caches (Project.Data.Tree);

      Iter := Start (Project.Data.Tree.Root,
                     Recursive => True);
      while Current (Iter) /= No_Project loop
         Trace (Me, "  " & Current (Iter).Project_Path.Display_Full_Name);

         if Current (Iter) = Imported_Project then
            Remove := False;
            exit;
         end if;

         Projects.Next (Iter);
      end loop;

      if Remove and then Dep_ID /= No_Name then
         Tree_Private_Part.Projects_Htable.Remove
           (Tree_Node.Projects_HT, Dep_ID);
      end if;
   end Remove_Imported_Project;

   ----------------------
   -- Reset_All_Caches --
   ----------------------

   procedure Reset_All_Caches (Tree : Project_Tree_Data_Access) is
      Cursor : Project_Htables.Cursor := Tree.Projects.First;
   begin
      while Has_Element (Cursor) loop
         Unchecked_Free (Element (Cursor).Data.Imported_Projects.Items);
         Unchecked_Free (Element (Cursor).Data.Importing_Projects);
         Next (Cursor);
      end loop;
   end Reset_All_Caches;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Tree                      : Project_Tree;
      Project                   : Project_Type'Class;
      Imported_Project_Location : GNATCOLL.VFS.Virtual_File;
      Packages_To_Check         : GNAT.Strings.String_List_Access := No_Packs;
      Errors                    : Error_Report := null;
      Use_Relative_Path         : Boolean := True;
      Use_Base_Name             : Boolean := False;
      Limited_With              : Boolean := False)
      return Import_Project_Error
   is
      Tree_Node : constant GPR.Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      use GPR.Tree_Private_Part;

      procedure Fail (S : String);

      ----------
      -- Fail --
      ----------

      procedure Fail (S : String) is
      begin
         if Errors /= null then
            Errors (S);
         end if;
      end Fail;

      Basename         : constant Filesystem_String :=
                           Base_Name
                             (Imported_Project_Location,
                              Project_File_Extension);
      Imported_Project : Project_Node_Id := Empty_Project_Node;
      Dep_ID           : Name_Id;
      Dep_Name         : GPR.Tree_Private_Part.Project_Name_And_Node;
      Error            : Import_Project_Error;

   begin
      if not Project.Is_Editable then
         raise Project_Not_Editable;
      end if;

      GPR.Output.Set_Special_Output (Fail'Unrestricted_Access);
      GPR.Com.Fail := Fail'Unrestricted_Access;

      Dep_ID := Get_String (+Basename);

      Dep_Name := Tree_Private_Part.Projects_Htable.Get
        (Tree_Node.Projects_HT, Dep_ID);

      if Dep_Name /= No_Project_Name_And_Node then
         --  ??? We used to compare on the build server, but that might not be
         --  necessary (and we do not have access to this information in
         --  GNATCOLL in any case).
         if not File_Equal
           (Format_Pathname
              (+Get_String (Path_Name_Of (Dep_Name.Node, Tree_Node))),
            Imported_Project_Location.Full_Name,
            Local_Host)
         then
            Fail
              ("A different project with the same name"
               & " already exists in the project tree.");
            GPR.Output.Cancel_Special_Output;
            GPR.Com.Fail := null;
            return Project_Already_Exists;
         else
            Imported_Project := Dep_Name.Node;
         end if;

      else
         Override_Flags (Tree.Data.Env.Env, Create_Flags (null, False));

         GPR.Part.Parse
           (Tree_Node, Imported_Project,
            +Full_Name (Imported_Project_Location),
            Packages_To_Check      => Packages_To_Check,
            Is_Config_File         => False,
            Current_Directory      => Get_Current_Dir,
            Env                    => Tree.Data.Env.Env);

         GPR.Err.Finalize;
      end if;

      if Imported_Project = Empty_Project_Node then
         Trace (Me, "Add_Imported_Project: imported project not found ("
                & Imported_Project_Location.Display_Full_Name & ")");
         GPR.Output.Cancel_Special_Output;
         GPR.Com.Fail := null;
         return Imported_Project_Not_Found;
      end if;

      Compute_Importing_Projects (Project, Project.Data.Tree.Root);
      Error := Add_Imported_Project
        (Tree                      => Project.Data.Tree,
         Project                   => Project,
         Imported_Project          =>
           Tree.Instance_From_Node (Tree, Imported_Project),
         Errors                    => Errors,
         Use_Relative_Path         => Use_Relative_Path,
         Use_Base_Name             => Use_Base_Name,
         Limited_With              => Limited_With);

      if Error = Success then
         Create_Project_Instances
           (Tree, Tree, With_View => False);
      end if;

      return Error;
   end Add_Imported_Project;

   --------------------------
   -- Add_Imported_Project --
   --------------------------

   function Add_Imported_Project
     (Project           : Project_Type;
      Imported_Project  : Project_Type;
      Errors            : Error_Report := null;
      Use_Relative_Path : Boolean := True;
      Use_Base_Name     : Boolean := False;
      Limited_With      : Boolean := False) return Import_Project_Error is
   begin
      if not Project.Is_Editable then
         raise Project_Not_Editable;
      end if;

      Compute_Importing_Projects (Project, Project.Data.Tree.Root);

      return GNATCOLL.Projects.Normalize.Add_Imported_Project
        (Tree                      => Project.Data.Tree,
         Project                   => Project,
         Imported_Project          => Imported_Project,
         Errors                    => Errors,
         Use_Relative_Path         => Use_Relative_Path,
         Use_Base_Name             => Use_Base_Name,
         Limited_With              => Limited_With);

      --  No need for Create_Project_Instances in this version, since the
      --  imported_project was already in memory.
   end Add_Imported_Project;

   ------------------------------
   -- Delete_Scenario_Variable --
   ------------------------------

   procedure Delete_Scenario_Variable
     (Tree                     : Project_Tree'Class;
      External_Name            : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True) is
   begin
      if not Tree.Root_Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return;
      end if;

      GNATCOLL.Projects.Normalize.Delete_Scenario_Variable
        (Tree.Data, Tree.Root_Project,
         External_Name, Keep_Choice, Delete_Direct_References);

      --  Mark all projects in the hierarchy as modified, since they are
      --  potentially all impacted.

      declare
         Cursor : Project_Htables.Cursor := Tree.Data.Projects.First;
      begin
         while Has_Element (Cursor) loop
            Element (Cursor).Set_Modified (True);
            Next (Cursor);
         end loop;
      end;
   end Delete_Scenario_Variable;

   -----------------
   -- Rename_Path --
   -----------------

   function Rename_Path
     (Self               : Project_Type;
      Old_Path           : GNATCOLL.VFS.Virtual_File;
      New_Path           : GNATCOLL.VFS.Virtual_File;
      Use_Relative_Paths : Boolean) return Boolean
   is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;
      return GNATCOLL.Projects.Normalize.Rename_Path
        (Self.Data.Tree, Self, Old_Path, New_Path, Use_Relative_Paths);
   end Rename_Path;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Tree : Project_Tree'Class;
      Name : String;
      Path : GNATCOLL.VFS.Virtual_File) return Project_Type
   is
      D       : constant Filesystem_String :=
                  Name_As_Directory (Path.Full_Name)
                  & (+Translate (To_Lower (Name), To_Mapping (".", "-")))
                  & GNATCOLL.Projects.Project_File_Extension;
      Project : constant Project_Node_Id :=
                  GPR.Tree.Create_Project
                    (In_Tree        => Tree.Data.Tree,
                     Name           => Get_String (Name),
                     Full_Path      => Path_Name_Type (Get_String (+D)),
                     Is_Config_File => False);
      P       : Project_Type;
   begin
      P := Tree.Instance_From_Node (Tree, Project);
      P.Set_Modified (True);
      return P;
   end Create_Project;

   --------------------------
   -- Set_Extended_Project --
   --------------------------

   procedure Set_Extended_Project
     (Self               : GNATCOLL.Projects.Project_Type;
      Extended           : GNATCOLL.Projects.Project_Type;
      Extend_All         : Boolean := False;
      Use_Relative_Paths : Boolean := False)
   is
   begin
      if not Self.Is_Editable then
         raise Project_Not_Editable;
      end if;
      if Use_Relative_Paths then
         declare
            Path : constant Filesystem_String :=
                     Relative_Path (Extended.Project_Path,
                                    Self.Project_Path);
         begin
            Set_Extended_Project_Path_Of
              (Self.Data.Node,
               Self.Data.Tree.Tree,
               To => Path_Name_Type (Get_String (+Path)));
         end;

      else
         Set_Extended_Project_Path_Of
           (Self.Data.Node,
            Self.Data.Tree.Tree,
            To => Path_Name_Type
              (Get_String (+Extended.Project_Path.Full_Name)));
      end if;

      Set_Extended_Project_Of
        (Project_Declaration_Of (Self.Data.Node, Self.Data.Tree.Tree),
         Self.Data.Tree.Tree,
         To => Extended.Node);

      if Extend_All then
         Set_Is_Extending_All (Self.Data.Node, Self.Data.Tree.Tree);
      end if;
   end Set_Extended_Project;

   ------------------------------
   -- Create_Scenario_Variable --
   ------------------------------

   function Create_Scenario_Variable
     (Project       : Project_Type;
      Name          : String;
      Type_Name     : String;
      External_Name : String) return Scenario_Variable
   is
      Tree_Node : constant GPR.Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      Typ, Var  : Project_Node_Id;
   begin
      if not Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return GNATCOLL.Projects.No_Variable;
      end if;

      GNATCOLL.Projects.Normalize.Normalize (Project.Data.Tree, Project);
      Typ := Create_Type (Tree_Node, Project.Data.Node, Type_Name);
      Var := Create_Typed_Variable
        (Tree_Node, Project.Data.Node, Name, Typ,
         Add_Before_First_Case_Or_Pkg => True);
      Set_Value_As_External (Tree_Node, Var, External_Name);

      Project.Set_Modified (True);

      --  Clear the cache
      Unchecked_Free (Project.Data.Tree.Env.Scenario_Variables);

      return (Ext_Name    => Get_String (External_Name),
              Var_Name    => No_Name,
              Default     => No_Name,
              Value       => No_Name,
              String_Type => Typ,
              Tree_Ref    => Tree_Node,
              First_Project_Path => GPR.No_Path);
   end Create_Scenario_Variable;

   --------------------------
   -- Change_External_Name --
   --------------------------

   procedure Change_External_Name
     (Tree     : Project_Tree'Class;
      Variable : in out Scenario_Variable;
      New_Name : String)
   is
      Tree_Node : constant GPR.Project_Node_Tree_Ref := Tree.Data.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the environment variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node, Tree_Node) = N_External_Value then
            Set_String_Value_Of
              (External_Reference_Of (Node, Tree_Node), Tree_Node,
               Get_String (New_Name));
         end if;
      end Callback;

      Ext_Ref : constant Name_Id := Get_String (External_Name (Variable));
   begin
      if not Tree.Root_Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return;
      end if;

      GNATCOLL.Projects.Normalize.Normalize (Tree.Data, Tree.Root_Project);
      For_Each_Environment_Variable
        (Tree.Data.Tree,
         Tree.Root_Project,
         Ext_Ref, No_Name, Callback'Unrestricted_Access);

      Tree.Root_Project.Set_Modified (True);

      --  Create the new variable, to avoid errors when computing the view of
      --  the project.
      Variable.Ext_Name := Get_String (New_Name);

      Tree.Change_Environment ((1 => Variable));
   end Change_External_Name;

   -----------------------
   -- Set_Default_Value --
   -----------------------

   procedure Set_Default_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Default       : String)
   is
      Tree_Node : constant GPR.Project_Node_Tree_Ref := Tree.Data.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the environment variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent, Choice);
      begin
         if Kind_Of (Node, Tree_Node) = N_Typed_Variable_Declaration then
            Set_External_Default_Of
              (Current_Term
                 (First_Term (Expression_Of (Node, Tree_Node), Tree_Node),
                  Tree_Node),
               Tree_Node,
               Enclose_In_Expression
                 (Create_Literal_String (Get_String (Default), Tree_Node),
                  Tree_Node));
         end if;
      end Callback;

   begin
      if not Tree.Root_Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return;
      end if;

      For_Each_Environment_Variable
        (Tree.Data.Tree, Tree.Root_Project,
         Get_String (External_Name), No_Name,
         Callback'Unrestricted_Access);
      Tree.Root_Project.Set_Modified (True);
   end Set_Default_Value;

   ------------------
   -- Rename_Value --
   ------------------

   procedure Rename_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Old_Value     : String;
      New_Value     : String)
   is
      Tree_N : constant GPR.Project_Node_Tree_Ref := Tree.Data.Tree;
      Old_V  : constant Name_Id := Get_String (Old_Value);
      New_V  : constant Name_Id := Get_String (New_Value);
      N      : constant Name_Id := Get_String (External_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the environment variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent);
         C : Project_Node_Id;
      begin
         case Kind_Of (Node, Tree_N) is
            when N_External_Value =>
               if External_Default_Of (Node, Tree_N) /= Empty_Project_Node
                 and then Expression_As_String
                   (Tree_N, External_Default_Of (Node, Tree_N)) = Old_V
               then
                  if Kind_Of (External_Default_Of (Node, Tree_N), Tree_N) =
                    N_Literal_String
                  then
                     Set_String_Value_Of
                       (External_Default_Of (Node, Tree_N), Tree_N, New_V);
                  else
                     Set_External_Default_Of
                       (Node, Tree_N, Create_Literal_String (New_V, Tree_N));
                  end if;
               end if;

            when N_String_Type_Declaration =>
               C := First_Literal_String (Node, Tree_N);
               while C /= Empty_Project_Node loop
                  if String_Value_Of (C, Tree_N) = Old_V then
                     Set_String_Value_Of (C, Tree_N, New_V);
                     exit;
                  end if;
                  C := Next_Literal_String (C, Tree_N);
               end loop;

            when N_Case_Item =>
               Set_String_Value_Of (Choice, Tree_N, New_V);

            when others =>
               null;
         end case;
      end Callback;

   begin
      if not Tree.Root_Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return;
      end if;

      GNATCOLL.Projects.Normalize.Normalize (Tree.Data, Tree.Root_Project);
      For_Each_Environment_Variable
        (Tree.Data.Tree, Tree.Root_Project,
         N, Old_V, Callback'Unrestricted_Access);

      if GPR.Ext.Value_Of (Tree.Data.Env.Env.External, N) /= No_Name
        and then GPR.Ext.Value_Of (Tree.Data.Env.Env.External, N) = Old_V
      then
         GPR.Ext.Add (Tree.Data.Env.Env.External, External_Name, New_Value,
                      GPR.Ext.From_Command_Line);
      end if;

      Tree.Root_Project.Set_Modified (True);
   end Rename_Value;

   ------------------
   -- Remove_Value --
   ------------------

   procedure Remove_Value
     (Tree          : Project_Tree'Class;
      External_Name : String;
      Value         : String)
   is
      Tree_N          : constant GPR.Project_Node_Tree_Ref := Tree.Data.Tree;
      Delete_Variable : exception;
      Type_Decl       : Project_Node_Id := Empty_Project_Node;
      V_Name          : constant Name_Id := Get_String (Value);
      Ext_Var         : constant Name_Id := Get_String (External_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the environment variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Choice);
         C, C2 : Project_Node_Id;
      begin
         case Kind_Of (Node, Tree_N) is
            when N_String_Type_Declaration =>
               Type_Decl := Node;

               C := First_Literal_String (Node, Tree_N);

               if Next_Literal_String (C, Tree_N) = Empty_Project_Node then
                  raise Delete_Variable;
               end if;

               if String_Value_Of (C, Tree_N) = V_Name then
                  Set_First_Literal_String
                    (Node, Tree_N, Next_Literal_String (C, Tree_N));
                  return;
               end if;

               loop
                  C2 := Next_Literal_String (C, Tree_N);
                  exit when C2 = Empty_Project_Node;

                  if String_Value_Of (C2, Tree_N) = V_Name then
                     Set_Next_Literal_String
                       (C, Tree_N, Next_Literal_String (C2, Tree_N));
                     exit;
                  end if;
                  C := C2;
               end loop;

            when N_External_Value =>
               if External_Default_Of (Node, Tree_N) /= Empty_Project_Node
                 and then String_Value_Of
                   (External_Default_Of (Node, Tree_N), Tree_N) = V_Name
               then
                  Set_External_Default_Of (Node, Tree_N, Empty_Project_Node);
               end if;

            when N_Case_Item =>
               C := First_Case_Item_Of
                 (Current_Item_Node (Parent, Tree_N), Tree_N);
               if C = Node then
                  Set_First_Case_Item_Of
                    (Current_Item_Node (Parent, Tree_N), Tree_N,
                     Next_Case_Item (C, Tree_N));
                  return;
               end if;

               loop
                  C2 := Next_Case_Item (C, Tree_N);
                  exit when C2 = Empty_Project_Node;

                  if C2 = Node then
                     Set_Next_Case_Item
                       (C, Tree_N, Next_Case_Item (C2, Tree_N));
                  end if;

                  C := C2;
               end loop;

            when others =>
               null;
         end case;
      end Callback;

   begin
      if not Tree.Root_Project.Is_Editable then
         Trace (Me, "Project is not editable");
         return;
      end if;

      GNATCOLL.Projects.Normalize.Normalize (Tree.Data, Tree.Root_Project);
      For_Each_Environment_Variable
        (Tree.Data.Tree, Tree.Root_Project, Ext_Var,
         Get_String (Value), Callback'Unrestricted_Access);

      --  Reset the value of the external variable if needed

      if GPR.Ext.Value_Of (Tree.Data.Env.Env.External, Ext_Var) = V_Name then
         if Type_Decl /= Empty_Project_Node then
            GPR.Ext.Add (Tree.Data.Env.Env.External,
                 External_Name,
                 Get_String (String_Value_Of
                               (First_Literal_String (Type_Decl, Tree_N),
                                Tree_N)),
                 GPR.Ext.From_Command_Line);
         else
            GPR.Ext.Add (Tree.Data.Env.Env.External, External_Name, "",
                         GPR.Ext.From_Command_Line);
         end if;
      end if;

      Tree.Root_Project.Set_Modified (True);

   exception
      when Delete_Variable =>
         Tree.Delete_Scenario_Variable
           (External_Name            => External_Name,
            Keep_Choice              => Value,
            Delete_Direct_References => False);
   end Remove_Value;

   ----------------
   -- Add_Values --
   ----------------

   procedure Add_Values
     (Tree     : Project_Tree'Class;
      Variable : Scenario_Variable;
      Values   : GNAT.Strings.String_List)
   is
      Tree_N         : constant GPR.Project_Node_Tree_Ref := Tree.Data.Tree;
      Type_Node, Var : Project_Node_Id;
      Iter           : Inner_Project_Iterator := Tree.Root_Project.Start;
      P              : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         if not P.Is_Editable then
            Trace (Me, "Project is not editable: " & P.Name);
            return;
         end if;

         GNATCOLL.Projects.Normalize.Normalize (Tree.Data, P);
         Var := Find_Scenario_Variable (Tree_N, P, External_Name (Variable));

         --  If variable is defined in the current project, then modify the
         --  type to Values.

         if Var /= Empty_Project_Node then
            Type_Node := String_Type_Of (Var, Tree_N);
            pragma Assert (Type_Node /= Empty_Project_Node);
            --  Set_First_Literal_String (Type_Node, Empty_Node);

            for J in Values'Range loop
               Add_Possible_Value (Tree_N, Type_Node, Values (J).all);
            end loop;

            P.Set_Modified (True);
         end if;

         Next (Iter);
      end loop;
   end Add_Values;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Project_Environment_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Environment'Class, Project_Environment_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Naming_Scheme_Record, Naming_Scheme_Access);
      NS : Naming_Scheme_Access;
   begin
      if Self /= null then
         while Self.Naming_Schemes /= null loop
            NS := Self.Naming_Schemes;
            Self.Naming_Schemes := NS.Next;

            Free (NS.Language);
            Free (NS.Default_Spec_Suffix);
            Free (NS.Default_Body_Suffix);
            Free (NS.Obj_Suffix);
            Unchecked_Free (NS);
         end loop;

         Unchecked_Free (Self.Predefined_Object_Path);
         Unchecked_Free (Self.Predefined_Source_Path);
         Unchecked_Free (Self.Predefined_Project_Path);
         Unchecked_Free (Self.Predefined_Source_Files);
         Free (Self.Xrefs_Subdir);
         Self.Extensions.Clear;
         Free (Self.Save_Config_File);
         Free (Self.Default_Gnatls);
         Free (Self.Gnatls);
         Free (Self.Forced_Target);
         Free (Self.Forced_Runtime);
         if not (Self.Packages_To_Check in All_Packs | No_Packs) then
            Free (Self.Packages_To_Check);
         end if;

         Free (Self.Env);

         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Project_Tree_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Tree_Data, Project_Tree_Data_Access);
   begin
      if Self /= null then
         if Self.Tree /= null then
            GPR.Tree_Private_Part.Project_Node_Table.Free
              (Self.Tree.Project_Nodes);
            Free (Self.Tree);
         end if;

         if not Self.Is_Aggregated then
            Self.Projects.Clear;
         end if;

         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Project_Tree_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Tree'Class, Project_Tree_Access);
   begin
      if Self /= null then
         Free (Self.Data);
         Unchecked_Free (Self);
      end if;
   end Free;

   ------------
   -- Status --
   ------------

   function Status (Self : Project_Tree) return Project_Status is
   begin
      if Self.Data = null then
         return Empty;
      else
         return Self.Data.Status;
      end if;
   end Status;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Self : Project_Tree; Status : Project_Status) is
   begin
      Trace (Me, "set project status to " & Status'Img);
      Self.Data.Status := Status;
   end Set_Status;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Path_Name_Array;
      Path : GPR.Path_Name_Type)
   is
      Tmp : Path_Name_Id_Array_Access;
   begin
      if Self.Items = null then
         Self.Items := new Path_Name_Id_Array (1 .. 4);
         Self.Last := 0;
      elsif Self.Last = Self.Items'Last then
         Tmp := Self.Items;
         Self.Items := new Path_Name_Id_Array (1 .. Self.Items'Last * 2);
         Self.Items (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Self.Last := Self.Last + 1;
      Self.Items (Self.Last) := Path;
   end Append;

   --------------------------
   -- Set_Save_Config_File --
   --------------------------

   procedure Set_Save_Config_File
      (Self : in out Project_Environment;
       Name : GNATCOLL.VFS.Filesystem_String) is
   begin
      Self.Save_Config_File := new String'(+Name);
   end Set_Save_Config_File;

   -----------------------------------------------
   -- Set_Disable_Use_Of_TTY_Process_Descriptor --
   -----------------------------------------------

   procedure Set_Disable_Use_Of_TTY_Process_Descriptor
      (Self : in out Project_Environment;
       Disabled : Boolean) is
   begin
      Self.TTY_Process_Descriptor_Disabled := Disabled;
   end Set_Disable_Use_Of_TTY_Process_Descriptor;

   ---------------------------
   -- Set_Host_Targets_List --
   ---------------------------

   procedure Set_Host_Targets_List is
      Gprbuild_Path   : Filesystem_String_Access;
      KB_Dir, TS_File : GNATCOLL.VFS.Virtual_File;

      KB : GPR.Knowledge.Knowledge_Base;

      use GPR.Knowledge;
      use GPR.Knowledge.String_Lists;

      TS_Id         : GPR.Knowledge.Targets_Set_Id;

      use DOM.Core, DOM.Core.Nodes;
      use Input_Sources.File;
      use Sax.Readers;
      use Schema.Dom_Readers;

      Input     : File_Input;
      Reader    : Schema.Dom_Readers.Tree_Reader;
      File_Node : DOM.Core.Node;
      N, N2     : DOM.Core.Node;

   begin
      Trace (Me, "Set_Host_Targets_List");
      if Host_Targets_List_Set then
         --  No point reparsing KB more than once.
         return;
      end if;

      Host_Targets_List_Set := True;

      Gprbuild_Path := Locate_Exec_On_Path ("gprbuild");
      if Gprbuild_Path = null then
         Trace (Me, "Gprbuild not found on path");
         return;
      end if;

      KB_Dir := Get_Parent (Create (Dir_Name (Gprbuild_Path.all)));
      KB_Dir := Join (Join (KB_Dir, "share"), "gprconfig");
      Free (Gprbuild_Path);
      GPR.Knowledge.Parse_Knowledge_Base
        (KB, KB_Dir.Display_Full_Name, Parse_Compiler_Info => False);
      GPR.Knowledge.Get_Targets_Set (KB, GPR.Sdefault.Hostname, TS_Id);

      Host_Targets_List := GPR.Knowledge.Get_Fallback_List
        (Base      => KB,
         On_Target => TS_Id);
      Host_Targets_List.Append
        (GPR.Knowledge.Normalized_Target (KB, TS_Id));
      GPR.Knowledge.Free_Knowledge_Base (KB);

      TS_File := Join (KB_Dir, "targetset.xml");
      if not TS_File.Is_Regular_File then
         Trace (Me, "targetset.xml not found");
         return;
      end if;

      Open (TS_File.Display_Full_Name, Input);
      Reader.Set_Feature (Schema_Validation_Feature, False);
      Reader.Set_Feature (Validation_Feature, False);  --  Do not use DTD
      Parse (Reader, Input);
      Close (Input);
      File_Node := DOM.Core.Documents.Get_Element (Get_Tree (Reader));

      if Node_Name (File_Node) = "gprconfig" then
         N := First_Child (File_Node);
         while N /= null loop
            if Node_Name (N) = "targetset" then
               declare
                  Attr : constant DOM.Core.Node :=
                    Get_Named_Item (Attributes (N), "canonical");
                  TS_Info : Targetset_Info;

                  use Ada.Strings.Unbounded;
               begin
                  if Attr /= null then
                     TS_Info.Canonical_Name :=
                       To_Unbounded_String (Node_Value (Attr));
                  end if;

                  N2 := First_Child (N);
                  while N2 /= null loop

                     if Node_Name (N2) = "target" then

                        if TS_Info.Canonical_Name = Null_Unbounded_String then
                           TS_Info.Canonical_Name :=
                             To_Unbounded_String
                               (Node_Value (First_Child (N2)));
                        end if;

                        TS_Info.Regexp_Imgs.Append
                          (Node_Value (First_Child (N2)));

                     end if;

                     N2 := Next_Sibling (N2);
                  end loop;

                  Normalization_Dictionary.Include (TS_Info);
               end;

            end if;
            N := Next_Sibling (N);
         end loop;
      end if;

      declare
         Doc : Document := Get_Tree (Reader);
      begin
         Free (Doc);
      end;
      Free (Reader);
   end Set_Host_Targets_List;

   -------------------------
   -- Aggregated_Projects --
   -------------------------

   function Aggregated_Projects
     (Project           : Project_Type;
      Unwind_Aggregated : Boolean := True) return Project_Array_Access
   is
      P          : Project_Type;
      Aggregated : Aggregated_Project_List;

      P_Files_Agg : Project_Array_Access;

      Result : Project_Array_Access :=
        new Project_Array'(Empty_Project_Array);

      procedure Append
        (Files : in out Project_Array_Access; P : Project_Type);

      procedure Append
        (Files : in out Project_Array_Access; P : Project_Type)
      is
         Tmp : Project_Array_Access;
      begin
         if Files = null then
            Files := new Project_Array'(1 => P);
         else
            Tmp := new Project_Array (1 .. Files'Length + 1);
            Tmp (1 .. Files'Length) := Files.all;
            Tmp (Tmp'Last) := P;
            Unchecked_Free (Files);
            Files := Tmp;
         end if;
      end Append;

   begin
      if Project.Get_View = GPR.No_Project then
         --  View has not been computed for this project.
         return Result;
      end if;

      if not Project.Is_Aggregate_Project then
         return Result;
      end if;

      Aggregated := Project.Data.View.Aggregated_Projects;
      while Aggregated /= null loop

         P := Project_Type
           (Project_From_Path
              (Project.Data.Tree_For_Map, Aggregated.Path));

         if Unwind_Aggregated and then P.Is_Aggregate_Project then
            P_Files_Agg := P.Aggregated_Projects;
            for P_File_Agg of P_Files_Agg.all loop
               Append (Result, P_File_Agg);
            end loop;
            Unchecked_Free (P_Files_Agg);
         else
            Append (Result, P);
         end if;

         Aggregated := Aggregated.Next;
      end loop;

      return Result;

   end Aggregated_Projects;

   ---------------------------
   -- Normalize_Target_Name --
   ---------------------------

   function Normalize_Target_Name (Target_Name : String) return String
   is
      use Ada.Strings.Unbounded;
   begin

      if Target_Name = "" then
         return "";
      end if;

      for TS_Info of Normalization_Dictionary loop

         for Regexp_Img of TS_Info.Regexp_Imgs loop
            declare
               Pattern : constant Pattern_Matcher :=
                 Compile ("^" & Regexp_Img & "$");
            begin
               if Match (Pattern, Target_Name) > Target_Name'First - 1 then
                  return To_String (TS_Info.Canonical_Name);
               end if;
            exception
               when Expression_Error =>
                  --  We do not care about possible errors, if the regexp is
                  --  bad we simply ignore it for normalization purposes.
                  null;
            end;
         end loop;

      end loop;

      return Target_Name;
   end Normalize_Target_Name;

begin
--     GPR.Initialize;
--     Csets.Initialize;
   Snames.Initialize;

   --  Disable verbose messages from project manager, not useful in GPS
   Opt.Quiet_Output := True;

   --  Unchecked_Shared_Lib_Imports is only relevant for builders
   Opt.Unchecked_Shared_Lib_Imports := True;
end GNATCOLL.Projects;
