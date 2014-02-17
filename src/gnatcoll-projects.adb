------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Text_IO;                 use Ada.Text_IO;

with GNAT.Case_Util;              use GNAT.Case_Util;
with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.Expect;                 use GNAT.Expect;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with GNATCOLL.Arg_Lists;          use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects.Normalize; use GNATCOLL.Projects.Normalize;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;          use GNATCOLL.VFS_Utils;

with Atree;
with Casing;                      use Casing;
with Csets;
pragma Warnings (Off);
pragma Warnings (Off, "*license of withed unit*");
with Krunch;
with Makeutl;
with Namet;                       use Namet;
with Osint;
with Opt;
with Output;
with Prj.Attr;                    use Prj.Attr;
with Prj.Com;
with Prj.Conf;                    use Prj.Conf;
with Prj.Env;                     use Prj, Prj.Env;
with Prj.Err;
with Prj.Ext;
with Prj.Part;
with Prj.Proc;
with Prj.PP;                      use Prj.PP;
with Prj.Tree;                    use Prj.Tree;
with Prj.Util;                    use Prj.Util;
with Sinput.P;
pragma Warnings (On, "*license of withed unit*");
pragma Warnings (On);
with Snames;                      use Snames;
with Types;                       use Types;

package body GNATCOLL.Projects is

   Me    : constant Trace_Handle := Create ("Projects", Default => Off);
   Debug : constant Trace_Handle := Create ("Projects.Debug", Default => Off);
   Me_Gnat : constant Trace_Handle :=
     Create ("Projects.GNAT", GNATCOLL.Traces.Off);
   Me_Aggregate_Support : constant Trace_Handle :=
     Create ("Projects.Aggregate", Default => Off);

   Dummy_Suffix : constant String := "<no suffix defined>";
   --  A dummy suffixes that is used for languages that have either no spec or
   --  no implementation suffix defined.

   Unknown_Importing_Projects : aliased constant Path_Name_Id_Array (1 .. 0) :=
     (others => <>);
   --  A dummy array used while computing importing projects

   package Virtual_File_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Virtual_File);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Scenario_Variable_Array, Scenario_Variable_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Type'Class, Project_Type_Access);

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
      Lang    : Namet.Name_Id;
      Source  : Prj.Source_Id;
      Next    : Source_File_Data_Access := null;
   end record;
   --  In some case, Lang might be set to Unknown_Language, if the file was
   --  set in the project (for instance through the Source_Files attribute),
   --  but no matching language was found.
   --  Next is only relevant when root project is aggregate project. In that
   --  case we can have multiple source files with same base name but different
   --  full names. Even sources with same full name can belong to different
   --  aggregated projects, so there are different Source_File_Data instances
   --  for such each such project;

   function Same_Source_And_Project (L, R : Source_File_Data) return Boolean;
   --  Returns true is both arguments have same full names and belong to
   --  the same project.

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

   type Project_Tree_Data is record
      Env       : Project_Environment_Access;

      Tree      : Prj.Tree.Project_Node_Tree_Ref;
      View      : Prj.Project_Tree_Ref;
      --  The description of the trees

      Status  : Project_Status := From_File;

      Root    : Project_Type := No_Project;
      --  The root of the project hierarchy

      Sources : Names_Files.Map;
      --  Index on base source file names, returns information about the file

      Objects_Basename : Names_Files.Map;
      --  The basename (with no extension or directory) of the object files.
      --  This is used to quickly filter out the relevant object or library
      --  files when an object directory is shared amongst multiple projects.
      --  This table does not point to the actual location of the object
      --  files, which might be in an extending project. It only provides a
      --  quick way to filter out irrelevant object files.

      Directories : Directory_Statuses.Map;
      --  Index on directory name
      --  CHANGE: might not be needed anymore, using the hash tables already
      --  in prj.*

      Projects : Project_Htables.Map;
      --  Index on project paths. This table is filled when the project is
      --  loaded.

      Scenario_Variables : Scenario_Variable_Array_Access;
      --  Cached value of the scenario variables. This should be accessed only
      --  through the function Scenario_Variables, since it needs to be
      --  initialized first.

      Timestamp : Ada.Calendar.Time := GNATCOLL.Utils.No_Time;
      --  Time when we last parsed the project from the disk
   end record;

   procedure Free (Self : in out Project_Tree_Data_Access);
   --  Free memory used by Self.

   function Get_View
     (Tree : Prj.Project_Tree_Ref; Name : Name_Id) return Prj.Project_Id;
   --  Return the project view for the project Name

   type External_Variable_Callback is access function
     (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean;
   --  Called for a typed variable declaration that references an external
   --  variable in Prj.
   --  Stops iterating if this subprogram returns False.

   type Get_Directory_Path_Callback is access function
     (Project : Prj.Project_Id) return Path_Information;
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
     (Tree     : Project_Node_Tree_Ref;
      Root     : Project_Node_Id;
      Callback : access procedure
        (Tree : Project_Node_Tree_Ref; Node : Project_Node_Id));
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
     (Project   : Project_Type;
      Recursive : Boolean;
      Callback  : External_Variable_Callback);
   --  Iterate other all the typed variable declarations that reference
   --  external variables in Project (or one of its imported projects if
   --  Recursive is true).
   --  Callback is called for each of them.

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
      return Prj.String_Element_Table.Table_Ptr;
   pragma Inline (String_Elements);
   --  Return access to the various tables that contain information about the
   --  project

   function Get_String (Id : Namet.File_Name_Type) return String;
   function Get_String (Id : Namet.Path_Name_Type) return String;
   pragma Inline (Get_String);
   --  Return the string in Name
   --  Same as Namet.Get_Name_String, but return "" in case of
   --  failure, instead of raising Assert_Failure.

   function Create_Flags
     (On_Error        : Prj.Error_Handler;
      Require_Sources : Boolean := True;
      Ignore_Missing_With : Boolean := False) return Prj.Processing_Flags;
   --  Return the flags to pass to the project manager in the context of GPS.
   --  Require_Sources indicates whether each language must have sources
   --  attached to it.

   function Length
     (Tree : Prj.Project_Tree_Ref; List : Prj.String_List_Id) return Natural;
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

   procedure Compute_Scenario_Variables (Tree : Project_Tree_Data_Access);
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
      Test_With_Missing_With : Boolean := True);
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
      Id      : Namet.Path_Name_Type;
      Xref_Dirs : Boolean) return Filesystem_String;
   --  Adds the object subdirectory to Id if one is defined

   function Kind_To_Part (Source : Source_Id) return Unit_Parts;
   --  Converts from Source.Kind to Unit_Parts

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
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator;
   --  Return the next item in the list

   function Data
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return Namet.Name_Id;
   --  Return the value pointed to by Iter.
   --  This could be either a N_String_Literal or a N_Expression node in the
   --  first case.
   --  The second case only works if Iter points to N_String_Literal.

   function Value_Of
     (Tree : Project_Node_Tree_Ref;
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
         if Project = Prj.No_Project then
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
            P := Project_Type (Project_From_Name (Tree.Data, Project.Name));
            P.Data.View_Is_Complete := False;
         end if;
      end if;
   end Mark_Project_Error;

   ---------------
   -- Tree_View --
   ---------------

   function Tree_View (P : Project_Type'Class) return Prj.Project_Tree_Ref is
   begin
      return P.Data.Tree.View;
   end Tree_View;

   ---------------
   -- Tree_Tree --
   ---------------

   function Tree_Tree
     (P : Project_Type'Class) return Prj.Tree.Project_Node_Tree_Ref
   is
   begin
      return P.Data.Tree.Tree;
   end Tree_Tree;

   ---------------------
   -- String_Elements --
   ---------------------

   function String_Elements
     (Data : Project_Tree_Data_Access)
      return Prj.String_Element_Table.Table_Ptr is
   begin
      return Data.View.Shared.String_Elements.Table;
   end String_Elements;

   ------------
   -- Length --
   ------------

   function Length
     (Tree : Prj.Project_Tree_Ref; List : Prj.String_List_Id) return Natural
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

   function Get_String (Id : Namet.File_Name_Type) return String is
   begin
      if Id = Namet.No_File then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end Get_String;

   function Get_String (Id : Namet.Path_Name_Type) return String is
   begin
      if Id = Namet.No_Path then
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

      elsif Get_View (Project) /= Prj.No_Project then
         return Get_String (Get_View (Project).Display_Name);

      else
         return Get_String
           (Prj.Tree.Name_Of (Project.Data.Node, Project.Tree_Tree));
      end if;
   end Name;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path
     (Project : Project_Type;
      Host    : String := Local_Host) return GNATCOLL.VFS.Virtual_File
   is
      View : constant Prj.Project_Id := Get_View (Project);
   begin
      if Project.Data = null or else Project.Data.Node = Empty_Node then
         return GNATCOLL.VFS.No_File;

      elsif View = Prj.No_Project then
         --  View=Prj.No_Project case needed for the project wizard

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
     (Project   : Project_Type;
      Recursive : Boolean := False) return GNATCOLL.VFS.File_Array
   is
      Current_Dir : constant Filesystem_String := Get_Current_Dir;
      Iter        : Inner_Project_Iterator := Start (Project, Recursive);
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
                  Recursive => True));
            Aggregated := Aggregated.Next;
         end loop;
         return Aggregated_Dirs.all;
      end if;

      loop
         P := Current (Iter);
         exit when P = No_Project;

         View := Get_View (P);
         exit when View = Prj.No_Project;

         Count := Count + Length (Project.Tree_View, View.Source_Dirs);
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
            exit when View = Prj.No_Project;

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
      Id        : Namet.Path_Name_Type;
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
      elsif Prj.Subdirs /= null then
         return Name_As_Directory
           (Path (Path'First .. Path'Last - Prj.Subdirs.all'Length - 1) &
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
      if View /= Prj.No_Project
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

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean := False;
      Including_Libraries : Boolean := False;
      Xrefs_Dirs          : Boolean := False) return File_Array
   is
      View : constant Project_Id := Get_View (Project);

   begin
      if View = Prj.No_Project then
         return (1 .. 0 => <>);

      elsif Recursive then
         declare
            Iter  : Project_Iterator := Start (Project, Recursive);
            Result : File_Array_Access;
            P     : Project_Type;
         begin
            loop
               P := Current (Iter);
               exit when P = No_Project or else P.Get_View = Prj.No_Project;

               Prepend (Result,
                        P.Object_Path
                          (Recursive           => False,
                           Including_Libraries => Including_Libraries,
                           Xrefs_Dirs          => Xrefs_Dirs));
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

         if View.Object_Directory = No_Path_Information
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

      function Find_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type) return Names_Files.Cursor;
      Local_Obj_Map : Names_Files.Map;
      --  Searches for Source_File_Data with given base name and a project from
      --  a project subtree that starts from Root.
      --  If the resulting Source_File_Data is not the first one in the list,
      --  it is placed in Local_Obj_Map and returning Coursor points to it.
      --  Local_Obj_Map must be cleared after each object file processed.

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

      ---------------------
      -- Find_In_Subtree --
      ---------------------

      function Find_In_Subtree
        (Map  : Names_Files.Map;
         Key  : GNATCOLL.VFS.Filesystem_String;
         Root : Project_Type) return Names_Files.Cursor
      is
         Cur  : Names_Files.Cursor;
         Iter : Inner_Project_Iterator;
         SFD  : Source_File_Data;
      begin
         Cur := Map.Find (Key);
         if Cur = Names_Files.No_Element then
            --  No object files with same base name expected for any project.
            return Cur;
         end if;

         Iter := Start (Extending_Project (Root, True));
         loop
            exit when Current (Iter) = No_Project;

            if Current (Iter) = Element (Cur).Project then
               --  First Source_File_Data is the one, we can return a cursor
               --  pointing at original map.
               return Cur;
            end if;

            Next (Iter);
         end loop;

         SFD := Element (Cur);
         loop
            exit when SFD.Next = null;
            SFD := SFD.Next.all;

            Iter := Start (Extending_Project (Root, True));
            loop
               exit when Current (Iter) = No_Project;

               if Current (Iter) = SFD.Project then
                  --  Creating a temporary element to point to.
                  Local_Obj_Map.Include (Key, SFD);
                  Cur := Local_Obj_Map.First;
                  return Cur;
               end if;

               Next (Iter);
            end loop;
         end loop;

         return Names_Files.No_Element;
      end Find_In_Subtree;

      Seen : Virtual_File_Sets.Set;

   begin
      if Is_Aggregate_Project (Self) then
         Increase_Indent (Me, "Library file for an aggregate project");
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

         Decrease_Indent (Me, "Done Library file for aggregate project");
         return;
      end if;

      if Active (Me) then
         Increase_Indent
           (Me, "Library_Files for project "
            & Self.Project_Path.Display_Full_Name);
      end if;

      if ALI_Ext (ALI_Ext'First) = '^' then
         Re := new Pattern_Matcher'(Compile (+ALI_Ext));
      end if;

      --  We do not call Object_Path with Recursive=>True, but instead
      --  iterate explicitly on the projects so that we can control which of
      --  the object_dir or library_dir we want to use *for each project*.
      --
      --  We are seeing extending projects before extended projects

      Prj_Iter := Self.Start_Reversed (Recursive => Recursive);
      loop
         Current_Project := Current (Prj_Iter);
         exit when Current_Project = No_Project;

         if Active (Me) then
            Trace (Me, "Current project: "
                   & Current_Project.Project_Path.Display_Full_Name);
         end if;

         declare
            Objects  : constant File_Array :=
              Object_Path (Current_Project,
                           Recursive           => False,
                           Including_Libraries => Including_Libraries,
                           Xrefs_Dirs          => Xrefs_Dirs);
            Dir : Virtual_File;
            Should_Append : Boolean;
         begin
            if Objects'Length > 0
              and then not Seen.Contains (Objects (Objects'First))
            then
               --  Only look at the first object directory (which is either
               --  object_dir, if it exists, or library_dir, if it exists).
               --  We never need to look at both of them.

               begin
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
                           B : constant Filesystem_String :=
                             Get_Base_Name (Tmp (F));
                           B_Last : Integer := B'Last;
                           Dot : Integer;
                           P, Lowest_Project   : Project_Type;
                        begin
                           Info_Cursor := Find_In_Subtree
                             (Self.Data.Tree.Objects_Basename, B, Self);

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
                                   Find_In_Subtree
                                     (Self.Data.Tree.Objects_Basename,
                                      B (B'First .. B_Last),
                                      Self);
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
                              Should_Append :=
                                Element (Info_Cursor).Project /= No_Project;

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
                                          Objs (Obj).Full_Name.all)
                                           .Is_Regular_File
                                       then
                                          if Active (Me) then
                                             Trace
                                               (Me, "overridden in project "
                                                & P.Name);
                                          end if;

                                          Lowest_Project := P;
                                          exit;
                                       end if;
                                    end loop;
                                 end;

                                 P := P.Extending_Project;
                              end loop For_Each_Extending_Project;

                              --  Since we are traversing each directory only
                              --  once, we cannot check that Lowest_Project is
                              --  Current_Project. Instead, we need to check
                              --  with the object dirs.

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

                        if Should_Append then
                           List.Append
                             (Library_Info'
                                (Library_File => Tmp (F),
                                 LI_Project => new Project_Type'
                                   (Current_Project),
                                 Source       => new File_Info'
                                   (Source_File_Data_To_Info
                                        (Element (Info_Cursor)))));
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
               end;
            end if;
         end;

         Next (Prj_Iter);
      end loop;

      if Include_Predefined then
         declare
            Predef : constant File_Array_Access :=
              Self.Data.Tree.Env.Predefined_Object_Path;
            Tmp : File_Array_Access;
         begin
            for P in Predef'Range loop
               if not Seen.Contains (Predef (P)) then
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

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out Library_Info_List) is
      L : Library_Info;
      C : Library_Info_Lists.Cursor := Self.First;
   begin
      while Library_Info_Lists.Has_Element (C) loop
         L := Library_Info_Lists.Element (C);
         Free (L.Source);
         Unchecked_Free (L.LI_Project);
         Library_Info_Lists.Next (C);
      end loop;

      Library_Info_Lists.Clear (Library_Info_Lists.List (Self)); --  inherited
   end Clear;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Self : in out File_Info_Set) is
      S : File_Info_Access;
      C : File_Info_Sets.Cursor := Self.First;
   begin
      while File_Info_Sets.Has_Element (C) loop
         S := File_Info_Sets.Element (C);
         Free (S);
         File_Info_Sets.Next (C);
      end loop;

      File_Info_Sets.Clear (File_Info_Sets.Set (Self)); --  inherited
   end Clear;

   --------------------------
   -- Direct_Sources_Count --
   --------------------------

   function Direct_Sources_Count (Project : Project_Type) return Natural is
   begin
      --  ??? Should directly use the size of Source_Files, since this is now
      --  precomputed when the project is loaded
      if Get_View (Project) = Prj.No_Project then
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
      Curs       : Names_Files.Cursor;
      File       : Virtual_File;
      Source     : Source_File_Data;
      Imports    : Boolean;
      Is_Limited : Boolean;
      Result     : File_Info;

   begin
      if Is_Absolute_Path (Name) then
         File := Create (Normalize_Pathname (Name, Resolve_Links => False));
         Result := Info (Tree => Self.Data.Tree, File => File);

         if Result.File = GNATCOLL.VFS.No_File then
            Result := File_Info'
              (File         => File,
               Project      => No_Project,
               Root_Project => Project_Type (Self),
               Part         => Unit_Separate,
               Name         => Namet.No_Name,
               Lang         => Namet.No_Name);
         end if;

         return Result;
      end if;

      --  Amongst all the files with the right basename, search the one, if
      --  any, that is visible from Self.

      Curs := Self.Data.Tree.Sources.Find (Name);
      if Has_Element (Curs) then
         --  Check amongst all possibilities which one is in Self or its
         --  imported projects.

         Source := Element (Curs);
         loop
            Imports := Source.Project = Project_Type (Self)
              or else Source.Project = No_Project;  --  predefined source file
            if not Imports then
               Self.Project_Imports
                 (Child            => Source.Project,
                  Include_Extended => True,
                  Imports          => Imports,
                  Is_Limited_With  => Is_Limited);
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
              (Self.Data.Tree.Sources, Name,
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
         Name         => Namet.No_Name,
         Lang         => Namet.No_Name);
   end Create_From_Project;

   ------------
   -- Create --
   ------------

   function Create
     (Self            : Project_Tree;
      Name            : Filesystem_String;
      Project         : Project_Type'Class := No_Project;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True;
      Ambiguous       : access Boolean := null)
      return GNATCOLL.VFS.Virtual_File
   is
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
      --  name than First_SFD.

      function Ambiguous_Base_Name
        (First_SFD : Source_File_Data) return Boolean
      is
         Next_SFD : Source_File_Data := First_SFD;
      begin
         loop
            if Next_SFD.File /= First_SFD.File then
               return True;
            end if;

            exit when Next_SFD.Next = null;
            Next_SFD := Next_SFD.Next.all;
         end loop;

         return False;
      end Ambiguous_Base_Name;

   begin
      if Ambiguous /= null then
         Ambiguous.all := False;
      end if;

      if Self.Data = null then
         --  No view computed, we do not even know the source dirs
         return GNATCOLL.VFS.No_File;
      end if;

      if Is_Absolute_Path (Name) then
         return Create (Normalize_Pathname (Name, Resolve_Links => False));
      end if;

      --  Is the file already in the cache ?
      --  This cache is automatically filled initially when the project is
      --  loaded, so we know that all source files of the project are in the
      --  cache and will be returned efficiently

      if Project.Data = null and then Use_Source_Path then
         Info_Cursor := Self.Data.Sources.Find (Base);

         if Has_Element (Info_Cursor) then
            if Ambiguous_Base_Name (Element (Info_Cursor)) then
               if Ambiguous /= null then
                  Ambiguous.all := True;
               end if;

               return GNATCOLL.VFS.No_File;
            else
               return Element (Info_Cursor).File;
            end if;
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
                  return GNATCOLL.VFS.No_File;
               end if;
            end if;

            Next (Iterator);
         end loop;
      end if;

      if Path /= GNATCOLL.VFS.No_File then
         --  Found single project with given base name.
         return Path;
      end if;

      --  We have to search in one or more projects

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
            return GNATCOLL.VFS.No_File;
         end if;

         if not Duplicate_Obj and then Use_Source_Path then
            --  No need to check for object duplicates in source dirs.
            Path := Locate_Regular_File
              (Name, Project2.Source_Dirs (Recursive => False));
         end if;

         if
           Use_Object_Path
           and then not Duplicate_Obj
           and then Path = GNATCOLL.VFS.No_File
         then
            --  We do not want to loose Path in the check fails.
            Path := Locate_Regular_File
              (Name, Project2.Object_Path
                 (Recursive => False, Including_Libraries => True));

            if
              Path /= GNATCOLL.VFS.No_File
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

      --  Only search in the predefined directories if the user did not
      --  specify an explicit project

      if Path = GNATCOLL.VFS.No_File and then Project.Data = null then
         if Use_Source_Path
           and then Self.Data.Env.Predefined_Source_Path /= null
         then
            Path := Locate_Regular_File
              (Name, Self.Data.Env.Predefined_Source_Path.all);
         end if;

         if Use_Object_Path
           and then Path = GNATCOLL.VFS.No_File
           and then Self.Data.Env.Predefined_Object_Path /= null
         then
            Path := Locate_Regular_File
              (Name, Self.Data.Env.Predefined_Object_Path.all);
         end if;

         In_Predefined := Path /= GNATCOLL.VFS.No_File;
      end if;

      --  If still not found, search in the current directory

      if Path = GNATCOLL.VFS.No_File then
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
      then
         --  Language and Source are always unknown: if we had a source file,
         --  it would have been set in the cache while loading the project.
         --  However, for runtime files we do compute the language since these
         --  are likely to be source files

         Source_Info := Source_File_Data'
           (Project => Project2,
            File    => Path,
            Lang    => No_Name,
            Source  => null,
            Next => null);

         if In_Predefined then
            Source_Info.Lang := Get_String (Language (Info (Self.Data, Path)));
         end if;

         Include_File (Self.Data.Sources, Base, Source_Info);
      end if;

      return Path;
   end Create;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Project   : Project_Type;
      Recursive : Boolean := False) return GNATCOLL.VFS.File_Array_Access
   is
      Count   : Natural;
      Index   : Natural := 1;
      P       : Project_Type;
      Sources : File_Array_Access;
      Ret     : File_Array_Access;

   begin
      if not Recursive then
         if Project.Data = null or else Project.Data.Files = null then
            return new File_Array (1 .. 0);
         else
            return new File_Array'(Project.Data.Files.all);
         end if;
      end if;

      declare
         Iter : Project_Iterator := Start (Project, Recursive);
      begin
         Count := 0;

         --  Count files

         loop
            P := Current (Iter);
            exit when P = No_Project;

            --  Files may be null in case of a parse error

            if P.Data.Files /= null then
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

            if P.Data.Files /= null then
               for S in P.Data.Files'Range loop
                  Sources (Index) := P.Data.Files (S);
                  Index := Index + 1;
               end loop;
            end if;

            Next (Iter);
         end loop;

         Ret := new File_Array'(Sources (Sources'First .. Index - 1));
         Unchecked_Free (Sources);
         return Ret;
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
            Name         => Namet.No_Name,
            Lang         => Namet.No_Name);
      end if;

      --  Lookup in the project's Source_Paths_HT, rather than in
      --  Registry.Data.Sources, since the latter does not support duplicate
      --  base names. In Prj.Nmsc, names have been converted to lower case on
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
               Name         => Namet.No_Name,
               Lang         => Name_Ada);

         elsif Ext = ".adb" then
            return File_Info'
              (Project      => No_Project,
               Root_Project => Tree.Root,
               File         => File,
               Part         => Unit_Body,
               Name         => Namet.No_Name,
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
            Lang := Namet.No_Name;
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
         Name         => Namet.No_Name,
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

   ---------------
   -- Info_List --
   ---------------

   function Info_Set
     (Self : Project_Tree'Class; File : GNATCOLL.VFS.Virtual_File)
      return File_Info_Set
   is
      M_Cur  : Names_Files.Cursor;
      B_Name : constant Filesystem_String := File.Base_Name;

      Source : Source_File_Data;
      S_Info : File_Info;

      Result : File_Info_Set :=
        (File_Info_Sets.Empty_Set with null record);

      function Unit_Kid_To_Part (Src_Kind : Prj.Source_Kind) return Unit_Parts;
      --  Translate Prj.Source_Kind into Unit_Parts.

      function Unit_Kid_To_Part (Src_Kind : Prj.Source_Kind) return Unit_Parts
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
      end Unit_Kid_To_Part;

   begin
      if Self.Data = null then
         raise Program_Error with "no project tree was parsed";
      end if;

      M_Cur := Self.Data.Sources.Find (B_Name);

      if M_Cur = Names_Files.No_Element then
         Result.Include (new File_Info'(Info (Self.Data, File)));
         return Result;
      end if;

      Source := Names_Files.Element (M_Cur);

      loop
         if Source.File = File then
            S_Info.Project := Source.Project;
            S_Info.Root_Project := Self.Root_Project;

            if Source.Source /= null then
               --  One of the initially cashed source files.
               S_Info.Part := Unit_Kid_To_Part (Source.Source.Kind);
               if Source.Source.Unit = No_Unit_Index then
                  --  Not applicable to C and other non unit-based languages.
                  S_Info.Name := No_Name;
               else
                  S_Info.Name := Source.Source.Unit.Name;
               end if;
               S_Info.Lang := Source.Source.Language.Name;

            else
               --  Cashed after a call to create, thus no Source_Id. The only
               --  thing known is the language.

               declare
                  Tmp_Info : constant File_Info := Info (Self.Data, File);
               begin
                  S_Info.Part := Tmp_Info.Part;
                  S_Info.Lang := Tmp_Info.Lang;
                  S_Info.Name := Tmp_Info.Name;
               end;
            end if;

            Result.Include (new File_Info'(S_Info));
         end if;

         exit when Source.Next = null;
         Source := Source.Next.all;
      end loop;

      if Result.Is_Empty then
         --  Same base name present, but no source with same full path.
         Result.Include (new File_Info'(Info (Self.Data, File)));
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

      function Is_Runtime_Unit return Boolean;
      --  Return True if Unit_Name is from the runtime

      ---------------------
      -- Is_Runtime_Unit --
      ---------------------

      function Is_Runtime_Unit return Boolean is
         Index : Natural := Unit_Name'First;
      begin
         if Unit_Name = "ada"
           or else Unit_Name = "interfaces"
           or else Unit_Name = "system"
           or else Unit_Name = "gnat"
         then
            return True;
         end if;

         while Index <= Unit_Name'Last
           and then Unit_Name (Index) /= '.'
         loop
            Index := Index + 1;
         end loop;

         return Index <= Unit_Name'Last and then
           (Unit_Name (Unit_Name'First .. Index - 1) = "ada" or else
            Unit_Name (Unit_Name'First .. Index - 1) = "interfaces" or else
            Unit_Name (Unit_Name'First .. Index - 1) = "system" or else
            Unit_Name (Unit_Name'First .. Index - 1) = "gnat");
      end Is_Runtime_Unit;

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
      if Is_Runtime_Unit then
         declare
            Buffer : String := Substitute_Dot (Unit_Name, "-");
            Len    : Natural := Buffer'Length;
         begin
            pragma Assert (Buffer'First = 1);
            Krunch (Buffer, Len, Maxlen => Buffer'Length,
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
         Name_Len := Unit_Name'Length;
         Name_Buffer (1 .. Name_Len) := Unit_Name;
         Unit := Name_Find;

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
               when All_Lower_Case => To_Lower (Uname);
               when All_Upper_Case => To_Upper (Uname);
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

               when others =>
                  return "";
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
      Info : constant File_Info := Self.Info (File);
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
      if Project_View = Prj.No_Project then
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
            Value := Value_Of (Index => Get_String (Index), In_Array => Elem,
                               Shared => Shared);
         end if;
      else
         Value := Value_Of (N, Var, Shared => Shared);
      end if;

      if Value = Nil_Variable_Value
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
      if Project = No_Project or else View = Prj.No_Project then
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
            return Prj.Image (Lang.Config.Naming_Data.Casing);
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
      if Value.Project = Prj.No_Project then
         return No_Project;
      else
         declare
            Name : constant String := Get_Name_String (Value.Project.Name);
         begin
            return Tree.Project_From_Name (Name);
         end;
      end if;
   end Attribute_Project;

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
      Shared         : constant Shared_Project_Tree_Data_Access :=
                         Project.Tree_View.Shared;
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
      N              : Name_Id;
   begin
      if Project_View = Prj.No_Project then
         return False;
      end if;

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
         return Value_Of (N, In_Arrays => Arr, Shared => Shared) /=
           No_Array_Element;
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
      Packages       : Prj.Package_Table.Table_Ptr;
      Array_Elements : Prj.Array_Element_Table.Table_Ptr;
      Pkg            : Package_Id := No_Package;
      Arr            : Array_Id;
      Elem, Elem2    : Array_Element_Id;
      N              : Name_Id;
      Count          : Natural := 0;

   begin
      if Project_View = Prj.No_Project then
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

   ---------------
   -- Languages --
   ---------------

   function Languages
     (Project : Project_Type; Recursive : Boolean := False) return String_List
   is
      Iter          : Inner_Project_Iterator := Start (Project, Recursive);
      Num_Languages : Natural := 0;
      Val           : Variable_Value;
      P             : Project_Type;

      procedure Add_Language
        (Lang : in out String_List; Index : in out Natural; Str : String);
      --  Add a new language in the list, if not already there

      ------------------
      -- Add_Language --
      ------------------

      procedure Add_Language
        (Lang : in out String_List; Index : in out Natural; Str : String) is
      begin
         for L in Lang'First .. Index - 1 loop
            if Lang (L).all = Str then
               return;
            end if;
         end loop;

         Lang (Index) := new String'(Str);
         Index := Index + 1;
      end Add_Language;

   begin
      if Get_View (Project) = Prj.No_Project then
         return GNAT.OS_Lib.Argument_List'(1 .. 1 => new String'("ada"));
      end if;

      loop
         P := Current (Iter);
         exit when P = No_Project;

         Val := Attribute_Value (P, String (Languages_Attribute));
         case Val.Kind is
            when Undefined => null;
            when Single    => Num_Languages := Num_Languages + 1;
            when List      =>
               Num_Languages :=
                 Num_Languages + Length (P.Tree_View, Val.Values);
         end case;

         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);

      declare
         --  If no project defines the language attribute, then they have
         --  Ada as an implicit language. Save space for it.
         Lang  : Argument_List (1 .. Num_Languages + 1);
         Index : Natural := Lang'First;
         Value : String_List_Id;
      begin
         loop
            P := Current (Iter);
            exit when P = No_Project;

            if not P.Has_Attribute (Languages_Attribute) then
               Add_Language (Lang, Index, "ada");

            else
               Val := Attribute_Value (P, String (Languages_Attribute));
               case Val.Kind is
                  when Undefined => null;
                  when Single    =>
                     Add_Language (Lang, Index, Get_Name_String (Val.Value));
                  when List      =>
                     Value := Val.Values;
                     while Value /= Nil_String loop
                        Add_Language
                          (Lang, Index,
                           Get_String
                             (String_Elements (P.Data.Tree)(Value).Value));
                        Value := String_Elements (P.Data.Tree)(Value).Next;
                     end loop;
               end case;
            end if;

            Next (Iter);
         end loop;

         return Lang (Lang'First .. Index - 1);
      end;
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
      if P /= Prj.No_Project then
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

   ------------------
   -- Is_Main_File --
   ------------------

   function Is_Main_File
     (Project        : Project_Type;
      File           : GNATCOLL.VFS.Filesystem_String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      Value : String_List_Access := Project.Attribute_Value (Main_Attribute);
   begin
      for V in Value'Range loop
         if Equal (Value (V).all, +File, Case_Sensitive => Case_Sensitive) then
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
        or else Get_View (Project) = Prj.No_Project
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
        (Project : Prj.Project_Id) return Path_Information;

      ----------------------------------
      -- Get_Exec_Directory_Callback  --
      ----------------------------------

      function Get_Exec_Directory_Callback
        (Project : Prj.Project_Id) return Path_Information is
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
        (Project : Prj.Project_Id) return Path_Information;

      ------------------------------
      -- Get_Library_Dir_Callback --
      ------------------------------

      function Get_Library_Dir_Callback
        (Project : Prj.Project_Id) return Path_Information is
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
        (Project : Prj.Project_Id) return Path_Information;

      ----------------------------------
      -- Get_Library_ALI_Dir_Callback --
      ----------------------------------

      function Get_Library_ALI_Dir_Callback
        (Project : Prj.Project_Id) return Path_Information is
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
     (Tree     : Project_Node_Tree_Ref;
      Root     : Project_Node_Id;
      Callback : access procedure
                   (Tree : Project_Node_Tree_Ref; Node : Project_Node_Id))
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
         if Seen.Find (Proj) = Project_Sets.No_Element then
            Seen.Include (Proj);

            Callback (Tree, Proj);

            while With_Clause /= Empty_Node loop
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
            if Extended /= Empty_Node then
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
      Count : Natural := 0;

      procedure Do_Count (Tree : Project_Node_Tree_Ref;
                          Node : Project_Node_Id);

      --------------
      -- Do_Count --
      --------------

      procedure Do_Count
        (Tree : Project_Node_Tree_Ref; Node : Project_Node_Id)
      is
         pragma Unreferenced (Tree, Node);
      begin
         Count := Count + 1;
      end Do_Count;

   begin
      if Project.Data /= null
         and then Project.Data.Imported_Projects = null
      then
         if Project.Data.Local_Node_Tree = null then
            For_Each_Project_Node
              (Project.Data.Tree.Tree, Project.Data.Node,
               Do_Count'Unrestricted_Access);
         else
            For_Each_Project_Node
              (Project.Data.Local_Node_Tree, Project.Data.Node,
               Do_Count'Unrestricted_Access);
         end if;

         declare
            Imports : Path_Name_Id_Array (1 .. Count);
            Index   : Integer := Imports'First;

            procedure Do_Add (T : Project_Node_Tree_Ref; P : Project_Node_Id);
            procedure Do_Add
              (T : Project_Node_Tree_Ref; P : Project_Node_Id) is
            begin
               Imports (Index) := Prj.Tree.Path_Name_Of (P, T);
               Index := Index + 1;
            end Do_Add;

         begin
            if Project.Data.Local_Node_Tree = null then
               For_Each_Project_Node
                 (Project.Data.Tree.Tree, Project.Data.Node,
                  Do_Add'Unrestricted_Access);
            else
               For_Each_Project_Node
                 (Project.Data.Local_Node_Tree, Project.Data.Node,
                  Do_Add'Unrestricted_Access);
            end if;
            Project.Data.Imported_Projects := new Path_Name_Id_Array'
              (Imports (Imports'First .. Index - 1));
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
      Iter_Inner : Inner_Project_Iterator;

      package Path_Sets is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Path_Sets;

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
      begin
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
         else

            --  For the regulat project (aggregated or root) do a full
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
                  Iter.Project_List.Append (Current (Iter_Inner));
                  Project_Paths.Include
                    (Current (Iter_Inner).Project_Path.Display_Full_Name);
               end if;

               Next (Iter_Inner);
            end loop;
         end if;
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
            Current       => Root_Project.Data.Imported_Projects'First - 1);
         Next (Iter);
         return Iter;
      else
         return Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,  --  irrelevant
            Include_Extended => Include_Extended,
            Current          => Root_Project.Data.Imported_Projects'First);
      end if;
   end Start_Reversed;

   -----------
   -- Start --
   -----------

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Project_Iterator
   is
      Iter       : Project_Iterator;
      Iter_Inner : Inner_Project_Iterator;

      package Path_Sets is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Path_Sets;

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
      begin
         if Is_Aggregate_Project (Project) then
            --  processing aggregated project hierarchies
            Aggregated := Project.Data.View.Aggregated_Projects;

            while Aggregated /= null loop
               P := Project_Type
                 (Project_From_Path (Project.Data.Tree, Aggregated.Path));

               if Direct_Only then
                  if Project_Paths.Find (P.Project_Path.Display_Full_Name) =
                      Path_Sets.No_Element
                  then
                     --  we only need projects that are not yet in the list
                     Iter.Project_List.Append (P);
                     Project_Paths.Include (P.Project_Path.Display_Full_Name);
                  end if;

               else
                  Add_Project (P);
               end if;

               Aggregated := Aggregated.Next;
            end loop;

            --  aggregate project goes last in straight order
            if Project_Paths.Find
              (Project.Project_Path.Display_Full_Name) = Path_Sets.No_Element
            then
               Iter.Project_List.Append (Project_Type (Project));
               Project_Paths.Include (Project.Project_Path.Display_Full_Name);
            end if;

         else
            --  For the regulat project (aggregated or root) do a full
            --  iteration placing projects in the list.
            Iter_Inner :=
              Start
                (Root_Project     => Project,
                 Recursive        => Recursive,
                 Direct_Only      => Direct_Only,
                 Include_Extended => Include_Extended);

            while Current (Iter_Inner) /= No_Project loop
               if Project_Paths.Find
                 (Current (Iter_Inner).Project_Path.Display_Full_Name) =
                   Path_Sets.No_Element
               then
                  --  we only need projects that are not yet in the list
                  Iter.Project_List.Append (Current (Iter_Inner));
                  Project_Paths.Include
                    (Current (Iter_Inner).Project_Path.Display_Full_Name);
               end if;

               Next (Iter_Inner);
            end loop;
         end if;
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
      Assert (Me, Root_Project.Data /= null,
              "Start: Uninitialized project passed as argument");

      Compute_Imported_Projects (Root_Project);

      if Recursive then
         Iter := Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,
            Include_Extended => Include_Extended,
            Current        => Root_Project.Data.Imported_Projects'Last + 1);
         Next (Iter);
         return Iter;
      else
         return Inner_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Reversed         => False,  --  irrelevant
            Include_Extended => Include_Extended,
            Current          => Root_Project.Data.Imported_Projects'First);
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

      Tree_Tree   : Project_Node_Tree_Ref := Parent.Tree_Tree;
   begin
      Assert (Me, Child.Data /= null, "Project_Imports: no child provided");

      if Parent = No_Project then
         Imports := True;
         Is_Limited_With := False;
         return;
      end if;

      if Parent.Data.Local_Node_Tree /= null then
         --  Root project is aggregate project, we need to use the proper tree
         --  for the aggregated project.
         Tree_Tree := Parent.Data.Local_Node_Tree;
      end if;

      With_Clause := First_With_Clause_Of
           (Parent.Data.Node, Tree_Tree);

      while With_Clause /= Empty_Node loop
         if Project_Node_Of
           (With_Clause, Tree_Tree) = Child.Data.Node
         then
            Imports         := True;
            Is_Limited_With :=
              Non_Limited_Project_Node_Of (With_Clause, Tree_Tree)
              = Empty_Node;
            return;
         end if;

         With_Clause := Next_With_Clause_Of (With_Clause, Tree_Tree);
      end loop;

      --  Handling for extending projects ?

      if Include_Extended then
         Extended := Extended_Project_Of
           (Project_Declaration_Of (Parent.Data.Node, Tree_Tree),
            Tree_Tree);
         if Extended = Child.Data.Node then
            Imports := True;
            Is_Limited_With := False;
            return;
         end if;
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
                       Root_Project.Data.Imported_Projects;
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
         All_Prj := Root_Project.Data.Imported_Projects;
      end if;

      --  We consider that an extending project is "importing" its
      --  extended project, since it relies on it.

      declare
         Include   : Boolean_Array (All_Prj'Range) := (others => False);
         Was_Unknown : Boolean;

      begin
         for Index in All_Prj'Range loop
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
                    or else Parent.Data.Importing_Projects =
                      Unknown_Importing_Projects'Unrestricted_Access;

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

      if Project.Data.Local_Node_Tree /= null then
         --  choosing local tree for aggregated project
         Importing (Importing'Last) := Prj.Tree.Path_Name_Of
           (Project.Data.Node, Project.Data.Local_Node_Tree);
      else
         Importing (Importing'Last) := Prj.Tree.Path_Name_Of
           (Project.Data.Node, Project.Data.Tree.Tree);
      end if;

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
      Iter       : Project_Iterator;
      Iter_Inner : Inner_Project_Iterator;

      Local_Roots : Project_Lists.Vector := Project_Lists.Empty_Vector;

      package Path_Sets is new
        Ada.Containers.Indefinite_Ordered_Sets (String);
      use Path_Sets;

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

      if Is_Aggregate_Project (Project.Data.Tree.Root) then
         --  We need to look for importing projects in all trees created for
         --  each  directly aggregated project.
         Add_Local_Roots (Project.Data.Tree.Root);

         for I in Local_Roots.First_Index .. Local_Roots.Last_Index loop

            --  we need to reset importing projects for each local root
            Unchecked_Free (Project.Data.Importing_Projects);

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
         end loop;

         Iter.Project_Idx := Iter.Project_List.First_Index;
         return Iter;
      else

         Iter_Inner := Find_All_Projects_Importing
           (Project      => Project,
            Root_Project => Project.Data.Tree.Root,
            Include_Self => Include_Self,
            Direct_Only  => Direct_Only);

         loop
            exit when Current (Iter_Inner) = No_Project;

            Iter.Project_List.Append (Current (Iter_Inner));

            Next (Iter_Inner);
         end loop;
      end if;

      Iter.Project_Idx := Iter.Project_List.First_Index;
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

      Trace (Me, "Find_All_Projects_Importing " & Project.Name);

      if Project.Data.Importing_Projects = null then
         Compute_Imported_Projects (Root_Project);
         Compute_Importing_Projects (Project, Root_Project);
      end if;

      Iter := Inner_Project_Iterator'
        (Root             => Project,
         Direct_Only      => Direct_Only,
         Importing        => True,
         Reversed         => False,
         Include_Extended => True,   --  ??? Should this be configurable
         Current          => Project.Data.Importing_Projects'Last + 1);

      --  The project iself is always at index 'Last
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
      if
        Iterator.Project_List.To_Cursor (Iterator.Project_Idx) =
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
      P : Project_Type;
   begin
      if Iterator.Importing then
         if Iterator.Current >=
           Iterator.Root.Data.Importing_Projects'First
         then
            return Project_Type
              (Project_From_Path
                 (Iterator.Root.Data.Tree,
                  Iterator.Root.Data.Importing_Projects (Iterator.Current)));
         end if;

      elsif Iterator.Current > Iterator.Root.Data.Imported_Projects'Last then
         return No_Project;

      elsif Iterator.Current >= Iterator.Root.Data.Imported_Projects'First then
         P := Project_Type
           (Project_From_Path
              (Iterator.Root.Data.Tree,
               Iterator.Root.Data.Imported_Projects (Iterator.Current)));

         if P.Data = null then
            return No_Project;
         else
            return P;
         end if;
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
                 Iterator.Root.Data.Imported_Projects'Last
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
                 Iterator.Root.Data.Imported_Projects'First
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

   procedure Compute_Scenario_Variables (Tree : Project_Tree_Data_Access) is
      T     : constant Project_Node_Tree_Ref := Tree.Tree;
      List  : Scenario_Variable_Array_Access;
      Curr  : Positive;

      function Count_Vars return Natural;
      --  Return the number of scenario variables in tree

      function Register_Var
        (Variable : Project_Node_Id; Proj : Project_Node_Id) return Boolean;
      --  Add the variable to the list of scenario variables, if not there yet
      --  (see the documentation for Scenario_Variables for the exact rules
      --  used to detect aliases).

      function External_Default (Var : Project_Node_Id) return Name_Id;
      --  Return the default value for the variable. Var must be a variable
      --  declaration or a variable reference. This routine supports only
      --  single expressions (no composite values).

      ----------------
      -- Count_Vars --
      ----------------

      function Count_Vars return Natural is
         Count : Natural := 0;

         function Cb
           (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean;
         --  Increment the total number of variables

         --------
         -- Cb --
         --------

         function Cb
           (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean
         is
            pragma Unreferenced (Variable, Prj);
         begin
            Count := Count + 1;
            return True;
         end Cb;

      begin
         For_Each_External_Variable_Declaration
           (Tree.Root, Recursive => True, Callback => Cb'Unrestricted_Access);
         return Count;
      end Count_Vars;

      ----------------------
      -- External_Default --
      ----------------------

      function External_Default (Var : Project_Node_Id) return Name_Id is
         Proj : Project_Type    := Tree.Root;
         Expr : Project_Node_Id := Expression_Of (Var, T);
      begin
         Expr := First_Term   (Expr, T);
         Expr := Current_Term (Expr, T);

         if Kind_Of (Expr, T) = N_External_Value then
            Expr := External_Default_Of (Expr, T);

            if Expr = Empty_Node then
               return No_Name;
            end if;

            if Kind_Of (Expr, T) /= N_Literal_String then
               Expr := First_Term (Expr, T);
               Assert (Me, Next_Term (Expr, T) = Empty_Node,
                       "Default value cannot be a concatenation");

               Expr := Current_Term (Expr, T);

               if Kind_Of (Expr, T) = N_Variable_Reference then
                  --  A variable reference, look for the corresponding string
                  --  literal.

                  declare
                     Var    : constant Name_Id :=
                                Prj.Tree.Name_Of (Expr, T);
                     In_Prj : constant Project_Node_Id :=
                                Project_Node_Of (Expr, T);
                     Decl   : Project_Node_Id;
                  begin
                     if In_Prj /= Empty_Node then
                        --  This variable is defined in another project, get
                        --  project reference.
                        Proj := Project_Type
                          (Project_From_Name
                             (Tree, Prj.Tree.Name_Of (In_Prj, T)));
                     end if;

                     --  Look for Var declaration into the project

                     Decl := First_Declarative_Item_Of
                       (Project_Declaration_Of (Proj.Data.Node, T), T);

                     while Decl /= Empty_Node loop
                        Expr := Current_Item_Node (Decl, T);

                        if Prj.Tree.Name_Of (Expr, T) = Var then
                           Expr := Expression_Of (Expr, T);
                           Expr := First_Term (Expr, T);
                           --  Get expression and corresponding term

                           --  Check now that this is not a composite value

                           Assert
                             (Me, Next_Term (Expr, T) = Empty_Node,
                              "Default value cannot be a concatenation");

                           --  Get the string literal

                           Expr := Current_Term (Expr, T);
                           exit;
                        end if;
                        Decl := Next_Declarative_Item (Decl, T);
                     end loop;
                  end;
               end if;

               if Kind_Of (Expr, T) /= N_Literal_String then
                  Trace (Me, "Default value can only be literal string");
                  Proj.Data.Uses_Variables := True; --  prevent edition
                  return No_Name;
               end if;
            end if;

            return String_Value_Of (Expr, T);

         else
            return No_Name;
         end if;
      end External_Default;

      ------------------
      -- Register_Var --
      ------------------

      function Register_Var
        (Variable : Project_Node_Id; Proj : Project_Node_Id) return Boolean
      is
         pragma Unreferenced (Proj);
         V        : constant Name_Id := External_Reference_Of (Variable, T);
         N        : constant String := Get_String (V);
         Var      : Scenario_Variable;
         Is_Valid : Boolean;
      begin
         for Index in 1 .. Curr - 1 loop
            if External_Name (List (Index)) = N then
               --  Nothing to do
               return True;
            end if;
         end loop;

         Var := Scenario_Variable'
           (Name        => V,
            Default     => External_Default (Variable),
            String_Type => String_Type_Of (Variable, T),
            Value       => Prj.Ext.Value_Of
              (Tree.Env.Env.External, V,
               With_Default => External_Default (Variable)));

         List (Curr) := Var;

         --  Ensure the external reference actually exists and has a valid
         --  value.

         Is_Valid := Prj.Ext.Value_Of
            (Tree.Env.Env.External, Var.Name) /= No_Name;

         if Is_Valid then
            declare
               Current : constant Name_Id :=
                  Prj.Ext.Value_Of (Tree.Env.Env.External, Var.Name);
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
               Prj.Ext.Add
                  (Tree.Env.Env.External, N, Get_Name_String (Var.Default),
                   Prj.Ext.From_Command_Line);
            else
               Prj.Ext.Add
                 (Tree.Env.Env.External, N,
                  Get_Name_String
                     (String_Value_Of
                        (First_Literal_String (Var.String_Type, T), T)),
                  Prj.Ext.From_Command_Line);
            end if;
         end if;

         Curr := Curr + 1;
         return True;
      end Register_Var;

   begin
      Trace (Me, "Compute the list of scenario variables");
      Unchecked_Free (Tree.Scenario_Variables);
      List :=  new Scenario_Variable_Array (1 .. Count_Vars);
      Curr := List'First;

      For_Each_External_Variable_Declaration
        (Tree.Root, Recursive => True,
         Callback => Register_Var'Unrestricted_Access);

      if Curr > List'Last then
         Tree.Scenario_Variables := List;
      else
         Tree.Scenario_Variables :=
           new Scenario_Variable_Array'(List (1 .. Curr - 1));
         Unchecked_Free (List);
      end if;
   end Compute_Scenario_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Self : Project_Tree) return Scenario_Variable_Array is
   begin
      return Scenario_Variables (Self.Data);
   end Scenario_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Tree : Project_Tree_Data_Access) return Scenario_Variable_Array is
   begin
      if Tree.Scenario_Variables = null then
         Compute_Scenario_Variables (Tree);
      end if;

      for V in Tree.Scenario_Variables'Range loop
         Tree.Scenario_Variables (V).Value :=
           Prj.Ext.Value_Of
             (Tree.Env.Env.External, Tree.Scenario_Variables (V).Name,
              With_Default => Tree.Scenario_Variables (V).Default);
      end loop;

      return Tree.Scenario_Variables.all;
   end Scenario_Variables;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables
     (Self : Project_Tree; External_Name : String)
      return Scenario_Variable
   is
      E : constant String := External_Name;
      Ext  : Name_Id;
      List : Scenario_Variable_Array_Access;
      Var  : Scenario_Variable;
   begin
      Ext := Get_String (E);

      if Self.Data.Scenario_Variables = null then
         Compute_Scenario_Variables (Self.Data);
      end if;

      for V in Self.Data.Scenario_Variables'Range loop
         if Self.Data.Scenario_Variables (V).Name = Ext then
            return Self.Data.Scenario_Variables (V);
         end if;
      end loop;

      Var := Scenario_Variable'
        (Name        => Ext,
         Default     => No_Name,
         String_Type => Empty_Node,   --   ??? Won't be able to edit it
         Value       => No_Name);

      List := Self.Data.Scenario_Variables;
      Self.Data.Scenario_Variables :=
        new Scenario_Variable_Array'
          (Self.Data.Scenario_Variables.all & Var);
      Unchecked_Free (List);

      return Var;
   end Scenario_Variables;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Var : Scenario_Variable) return String is
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

   ------------------------
   -- Change_Environment --
   ------------------------

   procedure Change_Environment
     (Self  : Project_Tree;
      Vars  : Scenario_Variable_Array) is
   begin
      for V in Vars'Range loop
         Prj.Ext.Add
           (Self.Data.Env.Env.External,
            Get_String (Vars (V).Name),
            Get_String (Vars (V).Value),
            Prj.Ext.From_Command_Line);
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
      Prj.Ext.Add
        (Self.Env.External, Name, Value,
         Prj.Ext.From_Command_Line);
   end Change_Environment;

   -----------
   -- Value --
   -----------

   function Value (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Value);
   end Value;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Tree : Prj.Project_Tree_Ref; Name : Name_Id) return Prj.Project_Id
   is
      Proj : Project_List := Tree.Projects;
   begin
      while Proj /= null loop
         if Proj.Project.Name = Name
           and then Proj.Project.Qualifier /= Configuration
         then
            return Proj.Project;
         end if;

         Proj := Proj.Next;
      end loop;

      return Prj.No_Project;
   end Get_View;

   --------------
   -- Get_View --
   --------------

   function Get_View (Project : Project_Type'Class) return Prj.Project_Id is
   begin
      if Project.Data = null or else Project.Data.Node = Empty_Node then
         return Prj.No_Project;

      elsif Project.Data.View = Prj.No_Project then
         Project.Data.View :=
           Get_View
             (Project.Tree_View,
              Prj.Tree.Name_Of (Project.Data.Node, Project.Tree_Tree));
      end if;

      return Project.Data.View;
   end Get_View;

   --------------------------------------------
   -- For_Each_External_Variable_Declaration --
   --------------------------------------------

   procedure For_Each_External_Variable_Declaration
     (Project   : Project_Type;
      Recursive : Boolean;
      Callback  : External_Variable_Callback)
   is
      Tree     : constant Prj.Tree.Project_Node_Tree_Ref := Project.Tree_Tree;
      Iterator : Inner_Project_Iterator := Start (Project, Recursive);
      P        : Project_Type;

      procedure Process_Prj (Prj : Project_Node_Id);
      --  Process all the declarations in a single project

      -----------------
      -- Process_Prj --
      -----------------

      procedure Process_Prj (Prj : Project_Node_Id) is
         Pkg : Project_Node_Id := Prj;
         Var : Project_Node_Id;
      begin
         --  For all the packages and the common section
         while Pkg /= Empty_Node loop
            Var := First_Variable_Of (Pkg, Tree);

            while Var /= Empty_Node loop
               if Kind_Of (Var, Tree) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Var, Tree)
                 and then not Callback (Var, Prj)
               then
                  exit;

               elsif Kind_Of (Var, Tree) = N_Variable_Declaration
                 or else
                   (Kind_Of (Var, Tree) = N_Typed_Variable_Declaration
                    and then not Is_External_Variable (Var, Tree))
               then
                  if Active (Debug) then
                     Trace (Me, "Uses variable in " & P.Name);
                     Pretty_Print
                       (Var, Tree, Backward_Compatibility => False);
                  end if;
                  P.Data.Uses_Variables := True;
               end if;

               Var := Next_Variable (Var, Tree);
            end loop;

            if Pkg = Prj then
               Pkg := First_Package_Of (Prj, Tree);
            else
               Pkg := Next_Package_In_Project (Pkg, Tree);
            end if;
         end loop;
      end Process_Prj;

   begin
      loop
         P := Current (Iterator);
         exit when P.Data = null;

         P.Data.Uses_Variables := False;
         Process_Prj (P.Data.Node);
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
      if Get_View (Project) /= Prj.No_Project then
         Makeutl.Get_Switches
           (Source_File  => File_Name_Type
              (Get_String (File.Display_Base_Name)),
            Source_Lang  => Get_String (Language),
            Source_Prj   => Project.Data.View,
            Pkg_Name     => Get_String (In_Pkg),
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
     (Tree : Project_Node_Tree_Ref;
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
      return Iter.Current = Empty_Node;
   end Done;

   ----------
   -- Next --
   ----------

   function Next
     (Tree : Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return String_List_Iterator is
   begin
      pragma Assert (Iter.Current /= Empty_Node);

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
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Iter : String_List_Iterator) return Namet.Name_Id is
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
      Tree  : constant Prj.Tree.Project_Node_Tree_Ref := Self.Data.Tree;
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

   function "=" (Prj1, Prj2 : Project_Type) return Boolean is
   begin
      if Prj1.Data = null then
         return Prj2.Data = null;
      elsif Prj2.Data = null then
         return False;
      else
         return Prj1.Data.Node = Prj2.Data.Node;
      end if;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : File_Info_Access) return Boolean is
   begin
      return L.Project.Project_Path < R.Project.Project_Path;
   end "<";

   ----------------------
   -- Extended_Project --
   ----------------------

   function Extended_Project
     (Project : Project_Type) return Project_Type
   is
      Tree     : constant Project_Node_Tree_Ref := Project.Tree_Tree;
      Extended : constant Project_Node_Id := Extended_Project_Of
        (Project_Declaration_Of (Project.Data.Node, Tree), Tree);
   begin
      if Extended = Empty_Node then
         return No_Project;
      else
         return Project_Type
           (Project_From_Name
              (Project.Data.Tree, Prj.Tree.Name_Of (Extended, Tree)));
      end if;
   end Extended_Project;

   -----------------------
   -- Extending_Project --
   -----------------------

   function Extending_Project
     (Project : Project_Type; Recurse : Boolean := False) return Project_Type
   is
      Tree   : Project_Node_Tree_Ref := Project.Tree_Tree;
      Extended, Extending : Project_Node_Id;
   begin
      if Project.Data.Local_Node_Tree /= null then
         --  Setting proper tree for aggregated projects.
         Tree := Project.Data.Local_Node_Tree;
      end if;

      Extended := Project.Data.Node;

      loop
         Extending := Extending_Project_Of
           (Project_Declaration_Of (Extended, Tree), Tree);

         exit when not Recurse;

         --  Case of following extension chain: if we reached the of the chain,
         --  go back one step (to the last non-empty node) and exit.

         if Extending = Empty_Node then
            Extending := Extended;
            exit;
         end if;

         --  Iterate

         Extended := Extending;
      end loop;

      if Extending = Empty_Node then
         return No_Project;
      else
         return Project_Type
           (Project_From_Path
              (Project.Data.Tree,
               Prj.Tree.Path_Name_Of (Extending, Tree)));
      end if;
   end Extending_Project;

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

      if View /= Prj.No_Project then
         Lang := View.Languages;
         while Lang /= null loop
            Suffix := Name_Id (Lang.Config.Naming_Data.Spec_Suffix);
            if Suffix /= No_Name
              and then Ends_With (+Filename, Get_Name_String (Suffix))
            then
               return Filename'Last - Natural (Length_Of_Name (Suffix));
            end if;

            Suffix := Name_Id (Lang.Config.Naming_Data.Body_Suffix);
            if Suffix /= No_Name
              and then Ends_With (+Filename, Get_Name_String (Suffix))
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
     (Project : Project_Type;
      File    : Filesystem_String) return Filesystem_String
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
            Ada_Main => False,
            Language => Get_Name_String (Main_Source.Language.Name),
            Include_Suffix => False);
         return +Get_String (Exec_Name);
      end if;
   end Executable_Name;

   ------------------
   -- Create_Flags --
   ------------------

   function Create_Flags
     (On_Error        : Prj.Error_Handler;
      Require_Sources : Boolean := True;
      Ignore_Missing_With : Boolean := False) return Processing_Flags is
   begin
      if Require_Sources then
         return Create_Flags
           (Report_Error               => On_Error,
            When_No_Sources            => Warning,
            Require_Sources_Other_Lang => True,
            Compiler_Driver_Mandatory  => False,
            Allow_Duplicate_Basenames  => True,
            Require_Obj_Dirs           => Warning,
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
            Require_Obj_Dirs           => Warning,
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
      if View /= Prj.No_Project then
         return View.Has_Multi_Unit_Sources;
      end if;
      return False;
   end Has_Multi_Unit_Sources;

   -----------------------
   -- Project_From_Name --
   -----------------------

   function Project_From_Name
     (Tree : Project_Tree_Data_Access;
      Name : Namet.Name_Id) return Project_Type'Class
   is
      P_Cursor, P_Found : Project_Htables.Cursor;
      Name_Found : Boolean := False;

      --  Name is a base name (for now), but the htable is indexed on the
      --  full path of the project. So we need to traverse all its elements.
      --  In the case of aggregate projects, we return No_Project if multiple
      --  projects match.

      Normalized : constant Filesystem_String :=
         Create (+Get_String (Name)).Base_Name
            (Suffix => +Prj.Project_File_Extension, Normalize => True);

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
         P_Cursor := Tree.Projects.First;

         if Project_Qualifier_Of (Tree.Root.Data.Node, Tree.Tree) =
           Prj.Aggregate
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

   -------------------------
   --  Project_From_Path  --
   -------------------------

   function Project_From_Path
     (Self : Project_Tree'Class;
      Path : Virtual_File) return Project_Type
   is
      Project_File : Virtual_File;
      P_Cursor     : Project_Htables.Cursor;
   begin
      Project_File := Create (Normalize_Pathname (Full_Name (Path)));
      P_Cursor := Self.Data.Projects.Find (Project_File);

      if P_Cursor = Project_Htables.No_Element then
         return No_Project;
      end if;

      return Element (P_Cursor);
   end Project_From_Path;

   -------------------------
   --  Project_From_Path  --
   -------------------------

   function Project_From_Path
     (Tree    : Project_Tree_Data_Access;
      Path_Id : Path_Name_Type) return Project_Type'Class
   is
      P_Cursor : constant Project_Htables.Cursor :=
        Tree.Projects.Find (Create (+Get_String (Path_Id)));
   begin
      if P_Cursor = Project_Htables.No_Element then
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

   -----------------------
   -- Set_Object_Subdir --
   -----------------------

   procedure Set_Object_Subdir
     (Self   : in out Project_Environment;
      Subdir : GNATCOLL.VFS.Filesystem_String)
   is
      pragma Unreferenced (Self);
   begin
      Free (Prj.Subdirs);
      if Subdir = "." then
         Prj.Subdirs := null;
      else
         Prj.Subdirs := new String'(+Subdir);
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
      if Prj.Subdirs = null then
         return "";
      else
         return +Prj.Subdirs.all;
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
      Unchecked_Free (Self.Imported_Projects);
      Unchecked_Free (Self.Importing_Projects);
      Reset_View (Self);
   end On_Free;

   ----------------
   -- Reset_View --
   ----------------

   procedure Reset_View (Self : in out Project_Data'Class) is
   begin
      Self.View := Prj.No_Project;
      --  No need to reset Self.Imported_Projects, since this doesn't
      --  change when the view changes.

      Unchecked_Free (Self.Non_Recursive_Include_Path);
      Unchecked_Free (Self.Files);

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
      Default_Body_Suffix : String)
   is
      Spec, Impl : String_Access;
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

      Self.Naming_Schemes := new Naming_Scheme_Record'
        (Language            => new String'(To_Lower (Language_Name)),
         Default_Spec_Suffix => Spec,
         Default_Body_Suffix => Impl,
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
      return Self.Data.Root;
   end Root_Project;

   -----------------------------
   -- Same_Source_And_Project --
   -----------------------------

   function Same_Source_And_Project (L, R : Source_File_Data) return Boolean is
   begin
      return L.Project = R.Project and then L.File = R.File;
   end Same_Source_And_Project;

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
      return Ada.Containers.Hash_Type (Prj.Tree.Hash (Node));
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
      Found_Elem  : Source_File_Data;
      Elem_Access : Source_File_Data_Access;

   begin
      M_Cur := Map.Find (Key);
      if M_Cur = Names_Files.No_Element then
         Names_Files.Include (Map, Key, Elem);
         return;
      end if;

      Found_Elem := Names_Files.Element (M_Cur);

      --  Check the first one with same base name since we might have to update
      --  it in the map,
      if Same_Source_And_Project (Found_Elem, Elem) then
         --  Exactly same file, nothing has to be done.
         return;
      else

         --  Another file with same base name.

         if Found_Elem.Next = null then
            --  We still have to update the element in map.

            Found_Elem.Next := new Source_File_Data'(Elem);
            Map.Replace (Key, Found_Elem);

         else
            --  Look through other files with same base name and add elem
            --  if not present.

            Elem_Access := Found_Elem.Next;
            loop

               if Same_Source_And_Project (Elem_Access.all, Elem) then
                  --  Same file, nothing to add.
                  return;
               end if;

               if Elem_Access.Next = null then

                  --  We're at the last one, no given file in map.

                  Elem_Access.Next := new Source_File_Data'(Elem);
                  return;

               else
                  Elem_Access := Elem_Access.Next;
               end if;
            end loop;
         end if;
      end if;
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
      Recompute_View     : Boolean := True)
   is
      Tmp : Project_Tree'Class := Self;  --  Must use same tag
      Previous_Project : Virtual_File;
      Previous_Status  : Project_Status;
      Success          : Boolean;
      Project          : Project_Node_Id;
      Project_File     : GNATCOLL.VFS.Virtual_File := Root_Project_Path;

   begin
      Increase_Indent
        (Me, "Load project " & Root_Project_Path.Display_Full_Name);

      if Active (Me_Gnat) then
         Prj.Current_Verbosity := Prj.High;
      end if;

      if Self.Data /= null and then Self.Data.Root /= No_Project then
         Previous_Project := Self.Root_Project.Project_Path;
         Previous_Status  := Self.Data.Status;

      else
         Previous_Project := GNATCOLL.VFS.No_File;
         Previous_Status  := Default;
      end if;

      if not Is_Regular_File (Root_Project_Path) then
         Trace (Me, "Load: " & Display_Full_Name (Root_Project_Path)
                & " is not a regular file");
         Project_File :=
            Create
              (Normalize_Pathname
                (Full_Name (Project_File) & Project_File_Extension,
                 Resolve_Links => False));
         if not Is_Regular_File (Project_File) then
            Decrease_Indent
              (Me, "Load: " & Display_Full_Name (Project_File)
               & " is not a regular file");

            if Errors /= null then
               Errors (Display_Full_Name (Root_Project_Path)
                       & " is not a regular file");
            end if;

            raise Invalid_Project;
         end if;
      end if;

      Tmp.Data := new Project_Tree_Data;

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

      Trace (Me, "Initial parsing to check the syntax");
      Internal_Load
        (Tmp, Project_File, Errors,
         Report_Syntax_Errors => True,
         Project              => Project,
         Packages_To_Check    => Packages_To_Check,
         Recompute_View       => False);

      Prj.Err.Initialize;  --  Clear errors

      if Project = Empty_Node then
         --  Reset the list of error messages, and keep current project
         --  unchanged

         if Self.Data = null then
            Self.Load_Empty_Project (Env => Tmp.Data.Env);
         end if;

         Free (Tmp.Data.View);
         Free (Tmp.Data);

         Decrease_Indent (Me, "empty_node after parsing the tree");
         raise Invalid_Project;
      end if;

      --  We know the project is syntactically correct, so we can go on with
      --  the processing (we can't reuse the previous parsing, because we need
      --  to Unload first.

      Self.Unload;

      if Self.Data = null then
         Self.Data := Tmp.Data;
      else
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
         Recompute_View       => Recompute_View);

      if Previous_Status = Default then
         Trace (Me, "Remove previous default project on disk, no longer used");
         Delete (Previous_Project, Success);
      end if;

      Decrease_Indent (Me, "End of Load project");
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
      Makeutl.Db_Switch_Args.Append (Name_Find);
   end Add_Config_Dir;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : out Project_Environment_Access) is
      Path : String_Access;
   begin
      if Self = null then
         Self := new Project_Environment;
         Prj.Tree.Initialize (Self.Env, Create_Flags (null));
         Prj.Env.Initialize_Default_Project_Path
           (Self.Env.Project_Path, Target_Name => "");
         Prj.Env.Get_Path (Self.Env.Project_Path, Path);
         Self.Predefined_Project_Path :=
           new File_Array'(From_Path (+Path.all));
      end if;
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Tree : in out Project_Tree'Class;
      Env  : Project_Environment_Access) is
   begin
      if Tree.Data = null then
         Tree.Data := new Project_Tree_Data;

         if Env = null then
            Initialize (Tree.Data.Env);
         else
            Tree.Data.Env := Env;
         end if;
      end if;

      if Tree.Data.Tree = null then
         Tree.Data.Tree := new Project_Node_Tree_Data;
      end if;

      Prj.Tree.Initialize (Tree.Data.Tree);

      if Tree.Data.View = null then
         Tree.Data.View := new Prj.Project_Tree_Data;
      end if;

      Prj.Initialize (Tree.Data.View);
   end Reset;

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

      Success : Boolean;
      Fd      : Process_Descriptor_Access;

   begin
      Increase_Indent
        (Me, "Executing " & Argument_List_To_String (Gnatls_Args.all));
      begin
         Success := True;

         declare
            Gnatls_Path : constant Virtual_File :=
              Locate_On_Path
                (+Gnatls_Args (Gnatls_Args'First).all);
         begin
            if Gnatls_Path = GNATCOLL.VFS.No_File then
               Success := False;

               Trace (Me, "Could not locate exec " &
                      Gnatls_Args (Gnatls_Args'First).all);

               if Errors /= null then
                  Errors ("Could not locate exec " &
                          Gnatls_Args (Gnatls_Args'First).all);
               end if;
            else
               Trace (Me, "Spawning " & (+Gnatls_Path.Full_Name));
               Fd := new Process_Descriptor;
               Non_Blocking_Spawn
                 (Fd.all,
                  +Gnatls_Path.Full_Name,
                  Gnatls_Args (2 .. Gnatls_Args'Last),
                  Buffer_Size => 0, Err_To_Out => True);
            end if;
         end;

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

         Decrease_Indent (Me);
         return;
      end if;

      if Fd /= null then
         declare
            S : constant String := Get_Command_Output (Fd);
         begin
            Trace (Me, "Output of gnatls is " & S);
            Set_Path_From_Gnatls_Output
              (Self,
               Output       => S,
               GNAT_Version => GNAT_Version);
         end;

         Unchecked_Free (Fd);
      end if;

      Free (Gnatls_Args);
      Decrease_Indent (Me);
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
         Current := new File_Array'(1 .. 0 => <>);
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
      end;

      F := L + 1;

      while F <= Output'Last loop
         L := EOL (Output (F .. Output'Last));

         if Starts_With (Output (F .. L - 1), "Source Search Path:") then
            Set_Context (Source_Path);

         elsif Starts_With (Output (F .. L - 1), "Object Search Path:") then
            Set_Context (Object_Path);

         elsif Starts_With (Output (F .. L - 1), "Project Search Path:") then
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
      Test_With_Missing_With : Boolean := True)
   is
      procedure On_Error is new Mark_Project_Error (Tree);
      --  Any error while parsing the project marks it as incomplete, and
      --  prevents direct edition of the project.

      procedure Fail (S : String);
      --  Replaces Osint.Fail

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

      Errout_Handling : Prj.Part.Errout_Mode := Prj.Part.Always_Finalize;
   begin
      Traces.Assert (Me, Tree.Data /= null, "Tree data initialized");

      Reset (Tree, Tree.Data.Env);

      Trace (Me, "Set project path to " & Predefined_Path);
      Initialize_Empty (Tree.Data.Env.Env.Project_Path);
      Prj.Env.Set_Path (Tree.Data.Env.Env.Project_Path, Predefined_Path);

      Project := Empty_Node;

      --  Make sure errors are reinitialized before load
      Prj.Err.Initialize;

      if Test_With_Missing_With then
         Errout_Handling := Prj.Part.Never_Finalize;
      end if;

      Output.Set_Special_Output (Output.Output_Proc (Errors));

      Prj.Com.Fail := Fail'Unrestricted_Access;

      Tree.Data.Root := No_Project;

      Sinput.P.Clear_Source_File_Table;
      Sinput.P.Reset_First;

      Override_Flags (Tree.Data.Env.Env,
                      Create_Flags
                        (On_Error'Unrestricted_Access,
                         Ignore_Missing_With => Test_With_Missing_With));
      Prj.Part.Parse
        (Tree.Data.Tree, Project,
         +Root_Project_Path.Full_Name,
         Packages_To_Check => Packages_To_Check,
         Errout_Handling   => Errout_Handling,
         Store_Comments    => True,
         Is_Config_File    => False,
         Env               => Tree.Data.Env.Env,
         Current_Directory => Get_Current_Dir);

      if not Active (Me_Aggregate_Support)
        and then Project /= Empty_Node
        and then Project_Qualifier_Of (Project, Tree.Data.Tree) =
        Prj.Aggregate
      then
         Trace (Me, "Aggregate projects are not supported");
         Fail ("Aggregate projects are not supported");
         Project := Empty_Node;
         Output.Cancel_Special_Output;
         return;
      end if;

      if Project /= Empty_Node
        and then Tree.Data.Tree.Incomplete_With
      then
         Trace (Me, "Could not find some with-ed projects");

         --  Some "with" were found that could not be resolved. Check whether
         --  the user has specified a "gnatlist" switch. For this, we need to
         --  do phase1 of the processing (ie not look for sources).

         declare
            Success : Boolean;
            Tmp_Prj : Project_Id;
            P       : Package_Id;
            Value   : Variable_Value;

         begin
            Prj.Proc.Process_Project_Tree_Phase_1
              (In_Tree                => Tree.Data.View,
               Project                => Tmp_Prj,
               Packages_To_Check      => Packages_To_Check,
               Success                => Success,
               From_Project_Node      => Project,
               From_Project_Node_Tree => Tree.Data.Tree,
               Env                    => Tree.Data.Env.Env,
               Reset_Tree             => True,
               On_New_Tree_Loaded     => null);

            if not Success then
               Trace (Me, "Processing phase 1 failed");
               Project := Empty_Node;
            else
               Trace (Me, "Looking for IDE'gnatlist attribute");

               P := Value_Of
                 (Name_Ide,
                  In_Packages => Tmp_Prj.Decl.Packages,
                  Shared      => Tree.Data.View.Shared);
               if P = No_Package then
                  Trace (Me, "No package IDE");
                  Project := Empty_Node;  --  ??? Should we free it

               else
                  Value := Value_Of
                    (Get_String ("gnatlist"),
                     Tree.Data.View.Shared.Packages.Table (P).Decl.Attributes,
                     Tree.Data.View.Shared);

                  if Value = Nil_Variable_Value then
                     Trace (Me, "No attribute IDE'gnatlist");
                     Project := Empty_Node;  --  ??? Should we free it
                  else
                     declare
                        Gnatls       : constant String :=
                                         Get_Name_String (Value.Value);
                        GNAT_Version : String_Access;
                     begin
                        Trace (Me, "gnatlist=" & Gnatls);

                        Tree.Data.Env.Set_Path_From_Gnatls
                          (Gnatls       => Gnatls,
                           GNAT_Version => GNAT_Version,
                           Errors       => Fail'Unrestricted_Access);
                        Free (GNAT_Version);
                     end;
                  end if;
               end if;
            end if;

            --  Reparse the tree so that errors are reported as usual
            --  (or not if the new project path solves the issue).

            Override_Flags
              (Tree.Data.Env.Env,
               Create_Flags
                 (On_Error'Unrestricted_Access,
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
               Test_With_Missing_With => False);

            Output.Cancel_Special_Output;

            return;
         end;

      elsif Project = Empty_Node
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
               Ignore_Missing_With => False));
         Internal_Load
           (Tree                   => Tree,
            Root_Project_Path      => Root_Project_Path,
            Errors                 => Errors,
            Report_Syntax_Errors   => Report_Syntax_Errors,
            Project                => Project,
            Recompute_View         => Recompute_View,
            Packages_To_Check      => Packages_To_Check,
            Test_With_Missing_With => False);
         return;

      elsif Test_With_Missing_With then
         Trace (Me, "Project parsed with success");

         --  We correctly parsed the project, but should finalize anyway
         if Report_Syntax_Errors then
            Prj.Err.Finalize;
         else
            Prj.Err.Initialize;
         end if;
      end if;

      Override_Flags (Tree.Data.Env.Env, Create_Flags (null));

      if Project /= Empty_Node then
         Tree.Data.Root :=
           Tree.Instance_From_Node (Tree, Project);

         --  Create the project instances, so that we can use the
         --  project_iterator (otherwise Current cannot return a project_type).
         --  These instances, for now, will have now view associated

         Create_Project_Instances
           (Tree, Tree, With_View => False);

         Tree.Set_Status (From_File);

         Prj.Com.Fail := null;
         Output.Cancel_Special_Output;

         if Recompute_View then
            Tree.Recompute_View (Errors => Errors);
         end if;
      end if;

   exception
      when Invalid_Project =>
         Prj.Com.Fail := null;
         Output.Cancel_Special_Output;
         raise;

      when E : others =>
         Trace (Me, E);
         Prj.Com.Fail := null;
         Output.Cancel_Special_Output;
         raise;
   end Internal_Load;

   ----------------
   -- Reset_View --
   ----------------

   procedure Reset_View (Tree : Project_Tree'Class) is
   begin
      Tree.Data.Sources.Clear;
      Tree.Data.Directories.Clear;
      Unchecked_Free (Tree.Data.Scenario_Variables);
   end Reset_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Self   : in out Project_Tree;
      Errors : Projects.Error_Report := null)
   is
      procedure Add_GPS_Naming_Schemes_To_Config_File
        (Config_File  : in out Project_Node_Id;
         Project_Tree : Project_Node_Tree_Ref);
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
        (Node_Tree : Project_Node_Tree_Ref;
         Tree : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project : Project_Id);
      --  Creates project instancies for given project tree.

      -------------------------------------------
      -- Add_GPS_Naming_Schemes_To_Config_File --
      -------------------------------------------

      procedure Add_GPS_Naming_Schemes_To_Config_File
        (Config_File  : in out Project_Node_Id;
         Project_Tree : Project_Node_Tree_Ref)
      is
         NS   : Naming_Scheme_Access := Self.Data.Env.Naming_Schemes;
         Attr : Project_Node_Id;
         pragma Unreferenced (Attr);
      begin
         if Config_File = Empty_Node then
            --  Create a dummy config file if none was found. In that case we
            --  need to provide the Ada naming scheme as well

            Trace (Me, "Creating dummy configuration file");

            Add_Default_GNAT_Naming_Scheme
              (Config_File, Project_Tree);

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

         while NS /= null loop
            Trace (Me, "Add naming scheme for " & NS.Language.all);
            if NS.Default_Spec_Suffix.all /= Dummy_Suffix then
               Attr := Create_Attribute
                 (Tree               => Project_Tree,
                  Prj_Or_Pkg         => Create_Package
                    (Tree    => Project_Tree,
                     Project => Config_File,
                     Pkg     => "naming"),
                  Kind               => Single,
                  Name               => Get_String ("spec_suffix"),
                  Index_Name         => Get_String (NS.Language.all),
                  Value              => Create_Literal_String
                    (Tree  => Project_Tree,
                     Str   => Get_String (NS.Default_Spec_Suffix.all)));
            end if;

            if NS.Default_Body_Suffix.all /= Dummy_Suffix then
               Attr := Create_Attribute
                 (Tree               => Project_Tree,
                  Prj_Or_Pkg         => Create_Package
                    (Tree    => Project_Tree,
                     Project => Config_File,
                     Pkg     => "naming"),
                  Kind               => Single,
                  Name               => Get_String ("body_suffix"),
                  Index_Name         => Get_String (NS.Language.all),
                  Value              => Create_Literal_String
                    (Tree  => Project_Tree,
                     Str   => Get_String (NS.Default_Body_Suffix.all)));
            end if;

            NS := NS.Next;
         end loop;
      end Add_GPS_Naming_Schemes_To_Config_File;

      -------------------------------
      -- Initialize_Source_Records --
      -------------------------------

      procedure Initialize_Source_Records is

         procedure For_Sources
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            With_State : in out Integer);

         -----------------
         -- For_Sources --
         -----------------

         procedure For_Sources
           (Project : Project_Id;
            Tree    : Project_Tree_Ref;
            With_State : in out Integer)
         is
            pragma Unreferenced (With_State);
            Iter : Source_Iterator := For_Each_Source
                     (In_Tree => Tree, Project => Project);
            Src  : Prj.Source_Id;
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
                  Makeutl.Initialize_Source_Record (Src);
               else
                  if Src.Language.Config.Kind = Unit_Based
                    and then Src.Kind = Impl
                    and then Makeutl.Is_Subunit (Src)
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
        (Node_Tree : Project_Node_Tree_Ref;
         Tree : Project_Tree_Ref;
         Project_Node : Project_Node_Id;
         Project : Project_Id)
      is
         pragma Unreferenced (Project);

         T : Project_Tree'Class := Self;
      begin
         T.Data := new Project_Tree_Data'
           (Env => Self.Data.Env,
            Tree => Node_Tree,
            View => Tree,
            Status => Self.Data.Status,
            Timestamp => Self.Data.Timestamp,
            others => <>);
         --  T.Data.Tree should be set before the instances can be created
         T.Data.Root := T.Instance_From_Node (Self, Project_Node);
         Create_Project_Instances (T, Self, With_View => False);
      end On_New_Tree_Loaded;

   begin
      Trace (Me, "Recomputing project view");
      Output.Set_Special_Output (Output.Output_Proc (Errors));

      --  The views stored in the projects are no longer valid, we should make
      --  sure they are not called.

      declare
         C : Project_Htables.Cursor := Self.Data.Projects.First;
      begin
         while Has_Element (C) loop
            Element (C).Data.View := Prj.No_Project;
            Next (C);
         end loop;
      end;

      Reset_View (Self);
      Prj.Initialize (Self.Data.View);

      --  Compute the list of scenario variables. This also ensures that
      --  the variables do exist in the environment, and therefore that
      --  we can correctly load the project.

      Compute_Scenario_Variables (Self.Data);

      Opt.Follow_Links_For_Files := not Self.Data.Env.Trusted_Mode;
      Opt.Follow_Links_For_Dirs  := not Self.Data.Env.Trusted_Mode;

      begin
         Flags := Create_Flags
           (On_Error'Unrestricted_Access, Require_Sources => False);

         --  Make sure errors are reinitialized before load
         Prj.Err.Initialize;

         Override_Flags (Self.Data.Env.Env, Flags);

         Trace (Me, "Configuration file is '"
                & Self.Data.Env.Config_File.Display_Full_Name & "' autoconf="
                & Self.Data.Env.Autoconf'Img);

         Process_Project_And_Apply_Config
           (Main_Project        => View,
            User_Project_Node   => Self.Root_Project.Data.Node,
            Config_File_Name    => Self.Data.Env.Config_File.Display_Full_Name,
            Autoconf_Specified  => Self.Data.Env.Autoconf,
            Project_Tree        => Self.Data.View,
            Project_Node_Tree   => Self.Data.Tree,
            Packages_To_Check   => null,
            Allow_Automatic_Generation => Self.Data.Env.Autoconf,
            Automatically_Generated    => Automatically_Generated,
            Config_File_Path           => Config_File_Path,
            Env                        => Self.Data.Env.Env,
            Normalized_Hostname        => "",
            On_Load_Config             =>
              Add_GPS_Naming_Schemes_To_Config_File'Unrestricted_Access,
            On_New_Tree_Loaded         =>
              On_New_Tree_Loaded'Unrestricted_Access);

         Override_Flags (Self.Data.Env.Env, Create_Flags (null));

      exception
         when Invalid_Config =>
            Override_Flags (Self.Data.Env.Env, Create_Flags (null));
            --  Error message was already reported via Prj.Err
            null;
      end;

      --  Backward compatibility: load the project even if there was a fatal
      --  error. However, the view might be partial...
      --    if View = null then
      --       raise Invalid_Project;
      --    end if;

      Trace (Me, "View has been recomputed");

      --  Now that we have the view, we can create the project instances

      if View = Prj.No_Project then
         --  There was an error, but we still want to manipulate that project
         Self.Data.Root.Data.View :=
           Get_View (Self.Data.View,
                     Name => Prj.Tree.Name_Of
                       (Self.Data.Root.Data.Node, Self.Data.Tree));
      else
         Self.Data.Root.Data.View := View;
      end if;

      Create_Project_Instances (Self, Self, With_View => True);

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

      Prj.Err.Finalize;
      Output.Cancel_Special_Output;

   exception
      --  We can get an unexpected exception (actually Directory_Error) if the
      --  project file's path is invalid, for instance because it was
      --  modified by the user.

      when Invalid_Project =>
         Trace (Me, "Could not compute project view");
         Prj.Err.Finalize;
         Output.Cancel_Special_Output;
         raise;

      when E : others =>
         Trace (Me, E);
         Prj.Err.Finalize;
         Output.Cancel_Special_Output;
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
         Create (+Get_String (Prj.Tree.Path_Name_Of (Node, Self.Data.Tree)));
      Data : constant Project_Data_Access := Tree_For_Map.Data_Factory;
      P    : Project_Type;
   begin
      Data.Tree := Tree_For_Map.Data;
      Data.Node := Node;
      P := Project_Type'(Ada.Finalization.Controlled with Data => Data);

      if Self /= Tree_For_Map then
         --  Different project trees mean that we're dealing with one of
         --  aggregated projects.
         P.Data.Local_Tree := Self.Data.View;
         P.Data.Local_Node_Tree := Self.Data.Tree;
      end if;
      Tree_For_Map.Data.Projects.Include (Path, P);
      return P;
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

      procedure Do_Project2 (T : Project_Node_Tree_Ref; P : Project_Node_Id);

      ----------------
      -- Do_Project --
      ----------------

      procedure Do_Project
        (Proj : Project_Id;
         Tree : Project_Tree_Ref;
         S    : in out Integer)
      is
         pragma Unreferenced (S, Tree);
         Name : constant String := Get_String (Proj.Name);
         Iter : Project_Htables.Cursor;
         P    : Project_Type;
         Path : Virtual_File;

      begin
         --  Ignore virtual extending projects
         if Name'Length < Virtual_Prefix'Length
           or else Name (1 .. Virtual_Prefix'Length) /= Virtual_Prefix
         then
            Path := Create (+Get_String (Proj.Path.Display_Name));
            Iter := Tree_For_Map.Data.Projects.Find (Path);

            if Active (Me) then
               Assert (Me, Has_Element (Iter),
                       "Create_Project_Instances must be called"
                       & " to create project_type");
            end if;

            if Has_Element (Iter) then
               P := Element (Iter);
               Reset_View (P.Data.all);
               P.Data.View := Proj;
            end if;
         end if;
      end Do_Project;

      -----------------
      -- Do_Project2 --
      -----------------

      procedure Do_Project2 (T : Project_Node_Tree_Ref; P : Project_Node_Id) is
         Path : constant Virtual_File :=
            Create (+Get_String (Prj.Tree.Path_Name_Of (P, T)));
         Proj : Project_Type;
         Iter : Project_Htables.Cursor;
      begin
         Iter := Tree_For_Map.Data.Projects.Find (Path);
         if not Has_Element (Iter) then
            Proj := Self.Instance_From_Node (Tree_For_Map, P);
            Proj.Data.Node := P;
            Reset_View (Proj.Data.all);

         else
            Element (Iter).Data.Node := P;
            if Self /= Tree_For_Map then
               --  We need to update local trees containing this project
               --  to be able to get source files later.
               Element (Iter).Data.Local_Tree      := Self.Data.View;
               Element (Iter).Data.Local_Node_Tree := Self.Data.Tree;
            end if;
            Reset_View (Element (Iter).Data.all);
         end if;
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
      Self.Unload;
      Reset (Self, Env);

      Node := Prj.Tree.Create_Project
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

   ------------------------
   -- Parse_Source_Files --
   ------------------------

   procedure Parse_Source_Files (Self : in out Project_Tree) is
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

   begin
      Increase_Indent (Me, "Parse source files");

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
               Prj.Err.Error_Msg
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

         if P.Data.Local_Tree = null then
            Source_Iter := For_Each_Source (Self.Data.View, Get_View (P));
         else
            Source_Iter := For_Each_Source (P.Data.Local_Tree, Get_View (P));

            if Active (Me) then
               Trace (Me, "Using tree from aggregated project "
                      & P.Project_Path.Display_Full_Name);
            end if;
         end if;
         loop
            Source := Element (Source_Iter);
            exit when Source = No_Source;

            --  Do not consider sources that are excluded

            if not Source.Locally_Removed then

               --  Get the absolute path name for this source
               Get_Name_String (Source.Path.Display_Name);

               declare
                  File : constant Virtual_File :=
                    Create (+Name_Buffer (1 .. Name_Len));
               begin
                  if Is_Aggregate_Project (Self.Data.Root) then
                     Include_File
                       (Self.Data.Sources,
                        Base_Name (File),
                        (P, File, Source.Language.Name, Source, null));
                  else
                     --  No point in all the checks for regular project.

                     Self.Data.Sources.Include
                       (Base_Name (File),
                        (P, File, Source.Language.Name, Source, null));
                  end if;

                  if Source.Object /= Namet.No_File then
                     declare
                        Base : constant Filesystem_String :=
                          Base_Name
                            (Filesystem_String
                               (Get_Name_String (Source.Object)),
                             ".o");
                     begin
                        --  We know the actual object file will be in either
                        --  P or one of its extending projects. We can't
                        --  compute this information now though, because the
                        --  sources might not have been compiled. So the final
                        --  computation is done directly in Library_Files.

                        if Source.Index = 0 then
                           if Is_Aggregate_Project (Self.Data.Root) then
                              Include_File
                                (Self.Data.Objects_Basename,
                                 Base,
                                 (P, File, Source.Language.Name, Source,
                                  null));
                           else
                              --  No point in all the checks for regular
                              --  project.

                              Self.Data.Objects_Basename.Include
                                (Base,
                                 (P, File, Source.Language.Name, Source,
                                  null));
                           end if;
                        else

                           if Is_Aggregate_Project (Self.Data.Root) then
                              Include_File
                                (Self.Data.Objects_Basename,
                                 Base & "~"
                                 & (+Image
                                   (Integer (Source.Index),
                                        Min_Width => 0)),
                                 (P, File, Source.Language.Name, Source,
                                  null));
                           else
                              --  No point in all the checks for regular
                              --  project.

                              Self.Data.Objects_Basename.Include
                                (Base & "~"
                                 & (+Image
                                   (Integer (Source.Index),
                                        Min_Width => 0)),
                                 (P, File, Source.Language.Name, Source,
                                  null));
                           end if;
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
            end if;

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
         end;

         Next (Iter);
      end loop;

      Decrease_Indent (Me, "Done parse source files");
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
         Data.Tree := null;
         Reset_View (Data.all);
         Data.Node := Empty_Node;
         Next (Iter);
      end loop;

      if Self.Data.View /= null then
         Reset (Self.Data.View);
      end if;

      Prj.Tree.Tree_Private_Part.Projects_Htable.Reset
        (Self.Data.Tree.Projects_HT);
      Sinput.P.Clear_Source_File_Table;
      Sinput.P.Reset_First;

      --  Reset the scenario variables.
      --  The issue is that a given variable might currently have a value, and
      --  then be used in another project where that value is now illegal.
      --  Do not reset if we have an empty project, since otherwise we lose the
      --  values set from the command line
      --  ??? Don't reset after all, this is too tricky to get right, and might
      --  be plain wrong in fact.

--        if Self.Data.Status /= Empty then
--           Prj.Ext.Reset (Self.Data.Tree);
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

   --------------------------
   -- Is_Aggregate_Project --
   --------------------------

   function Is_Aggregate_Project (Self : Project_Type) return Boolean is
   begin
      if Self.Data.Local_Tree = null then
         --  root project
         return Project_Qualifier_Of
           (Self.Data.Node, Self.Data.Tree.Tree) = Prj.Aggregate;
      else
         return Project_Qualifier_Of
           (Self.Data.Node, Self.Data.Local_Node_Tree) = Prj.Aggregate;
      end if;
   end Is_Aggregate_Project;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable (Project : Project_Type) return Boolean is
   begin
      return not Project.Data.Uses_Variables
        and then Project.Data.View_Is_Complete;
   end Is_Editable;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Namet.Finalize;
      Atree.Atree_Private_Part.Nodes.Free;
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
      procedure W_Char (C : Character);
      procedure W_Eol;
      procedure W_Str  (S : String);

      ------------
      -- W_Char --
      ------------

      procedure W_Char (C : Character) is
      begin
         Put (Pretty_Printer'Class (Self), C);
      end W_Char;

      -----------
      -- W_Eol --
      -----------

      procedure W_Eol is
      begin
         New_Line (Pretty_Printer'Class (Self));
      end W_Eol;

      -----------
      -- W_Str --
      -----------

      procedure W_Str (S : String) is
      begin
         Put (Pretty_Printer'Class (Self), S);
      end W_Str;

   begin
      Prj.PP.Pretty_Print
        (Project                            => Project.Data.Node,
         In_Tree                            => Project.Data.Tree.Tree,
         Increment                          => Increment,
         Eliminate_Empty_Case_Constructions =>
           Eliminate_Empty_Case_Constructions,
         Minimize_Empty_Lines               => False,
         W_Char                             => W_Char'Unrestricted_Access,
         W_Eol                              => W_Eol'Unrestricted_Access,
         W_Str                              => W_Str'Unrestricted_Access,
         Backward_Compatibility             => False,
         Id                                 => Project.Data.View);
   end Put;

   ----------
   -- Node --
   ----------

   function Node
     (Project : Project_Type'Class) return Prj.Tree.Project_Node_Id is
   begin
      return Project.Data.Node;
   end Node;

   ----------
   -- Tree --
   ----------

   function Tree
     (Data : Project_Tree_Data_Access) return Prj.Tree.Project_Node_Tree_Ref is
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
      GNATCOLL.Projects.Normalize.Delete_Attribute
        (Self.Data.Tree, Self, String (Attribute), Scenario, Index);
   end Delete_Attribute;

   procedure Delete_Attribute
     (Self      : Project_Type;
      Attribute : Attribute_Pkg_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "") is
   begin
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
      GNATCOLL.Projects.Normalize.Rename_And_Move
        (Self.Data.Tree, Self, New_Name, Directory, Errors);

      Self.Data.Tree.Projects.Delete (Old_Path);

      Self.Data.Tree.Projects.Include (Self.Project_Path, Self);

      if Self.Data.View /= Prj.No_Project then
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
      Pkg_Id    : Package_Node_Id := Empty_Package;
      Attr_Id   : Attribute_Node_Id;
      Attr_Kind : Defined_Attribute_Kind;
      Var_Kind  : Defined_Variable_Kind;
   begin
      --  Need to make sure the predefined packages are already declared, or
      --  the new one will be discarded.

      Prj.Attr.Initialize;

      if Pkg /= "" then
         Pkg_Id := Package_Node_Id_Of (Get_String (Pkg));
         if Pkg_Id = Empty_Package then
            Trace (Me, "Register_New_Package (" & Pkg & ")");
            Register_New_Package (Name  => Pkg, Id => Pkg_Id);
            if Pkg_Id = Empty_Package
              or else Pkg_Id = Unknown_Package
            then
               Trace (Me, "Error registering new package");
            end if;
         end if;
      end if;

      if Pkg_Id = Empty_Package then
         Attr_Id := Attribute_Node_Id_Of
           (Name        => Get_String (Name),
            Starting_At => Prj.Attr.Attribute_First);
      else
         Attr_Id := Attribute_Node_Id_Of
           (Name        => Get_String (Name),
            Starting_At => First_Attribute_Of (Pkg_Id));
      end if;

      if Is_List then
         Var_Kind := Prj.List;
      else
         Var_Kind := Prj.Single;
      end if;

      if Indexed then
         if Case_Sensitive_Index then
            Attr_Kind := Prj.Attr.Associative_Array;
         else
            Attr_Kind := Prj.Attr.Case_Insensitive_Associative_Array;
         end if;

         --  Priority is given to the registered type
         if Attr_Id /= Empty_Attribute then
            Attr_Kind := Attribute_Kind_Of (Attr_Id);
            if Attr_Kind = Attribute_Kind'(Single) then
               Attr_Kind := Prj.Attr.Associative_Array;
            end if;
         end if;
      else
         Attr_Kind := Attribute_Kind'(Single);
      end if;

      if Attr_Id = Empty_Attribute then
         if Pkg = "" then
            return "Project attributes cannot be added at the top level of"
              & " project files, only in packages";

         else
            if Active (Me) then
               Trace (Me, "Register_New_Attribute (" & Name
                      & ", " & Pkg & ", " & Attr_Kind'Img & ", "
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
      Tree        : constant Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      With_Clause : Project_Node_Id :=
                      First_With_Clause_Of (Project.Node, Tree);
      Next        : Project_Node_Id;
   begin
      --  ??? When the project is no longer found in the hierarchy, it should
      --  also be removed from the htable in Prj.Tree, so that another
      --  project by that name can be loaded.

      if With_Clause /= Empty_Node
        and then Prj.Tree.Name_Of (With_Clause, Tree) =
        Prj.Tree.Name_Of (Imported_Project.Node, Tree)
      then
         Set_First_With_Clause_Of
           (Project.Node, Tree, Next_With_Clause_Of (With_Clause, Tree));
      else
         loop
            Next := Next_With_Clause_Of (With_Clause, Tree);
            exit when Next = Empty_Node;

            if Prj.Tree.Name_Of (Next, Tree) =
              Prj.Tree.Name_Of (Imported_Project.Node, Tree)
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
   end Remove_Imported_Project;

   ----------------------
   -- Reset_All_Caches --
   ----------------------

   procedure Reset_All_Caches (Tree : Project_Tree_Data_Access) is
      Cursor : Project_Htables.Cursor := Tree.Projects.First;
   begin
      while Has_Element (Cursor) loop
         Unchecked_Free (Element (Cursor).Data.Imported_Projects);
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
      Tree_Node : constant Project_Node_Tree_Ref := Project.Data.Tree.Tree;
      use Prj.Tree.Tree_Private_Part;

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
      Imported_Project : Project_Node_Id := Empty_Node;
      Dep_ID           : Name_Id;
      Dep_Name         : Prj.Tree.Tree_Private_Part.Project_Name_And_Node;
      Error            : Import_Project_Error;

   begin
      Output.Set_Special_Output (Fail'Unrestricted_Access);
      Prj.Com.Fail := Fail'Unrestricted_Access;

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
            Output.Cancel_Special_Output;
            Prj.Com.Fail := null;
            return Project_Already_Exists;
         else
            Imported_Project := Dep_Name.Node;
         end if;

      else
         Override_Flags (Tree.Data.Env.Env, Create_Flags (null, False));

         Prj.Part.Parse
           (Tree_Node, Imported_Project,
            +Full_Name (Imported_Project_Location),
            Packages_To_Check      => Packages_To_Check,
            Is_Config_File         => False,
            Current_Directory      => Get_Current_Dir,
            Env                    => Tree.Data.Env.Env);

         Prj.Err.Finalize;
      end if;

      if Imported_Project = Empty_Node then
         Trace (Me, "Add_Imported_Project: imported project not found ("
                & Imported_Project_Location.Display_Full_Name & ")");
         Output.Cancel_Special_Output;
         Prj.Com.Fail := null;
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
                  Prj.Tree.Create_Project
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
      Tree_Node : constant Project_Node_Tree_Ref := Project.Data.Tree.Tree;
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

      Unchecked_Free (Project.Data.Tree.Scenario_Variables);

      return (Name        => Get_String (External_Name),
              Default     => No_Name,
              Value       => No_Name,
              String_Type => Typ);
   end Create_Scenario_Variable;

   --------------------------
   -- Change_External_Name --
   --------------------------

   procedure Change_External_Name
     (Tree     : Project_Tree'Class;
      Variable : in out Scenario_Variable;
      New_Name : String)
   is
      Tree_Node : constant Project_Node_Tree_Ref := Tree.Data.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

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
      Variable.Name := Get_String (New_Name);

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
      Tree_Node : constant Project_Node_Tree_Ref := Tree.Data.Tree;

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

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
      Tree_N : constant Project_Node_Tree_Ref := Tree.Data.Tree;
      Old_V  : constant Name_Id := Get_String (Old_Value);
      New_V  : constant Name_Id := Get_String (New_Value);
      N      : constant Name_Id := Get_String (External_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each mtching node for the env. variable

      --------------
      -- Callback --
      --------------

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id) is
         pragma Unreferenced (Project, Parent);
         C : Project_Node_Id;
      begin
         case Kind_Of (Node, Tree_N) is
            when N_External_Value =>
               if External_Default_Of (Node, Tree_N) /= Empty_Node
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
               while C /= Empty_Node loop
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

      if Prj.Ext.Value_Of (Tree.Data.Env.Env.External, N) /= No_Name
        and then Prj.Ext.Value_Of (Tree.Data.Env.Env.External, N) = Old_V
      then
         Prj.Ext.Add (Tree.Data.Env.Env.External, External_Name, New_Value,
                      Prj.Ext.From_Command_Line);
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
      Tree_N          : constant Project_Node_Tree_Ref := Tree.Data.Tree;
      Delete_Variable : exception;
      Type_Decl       : Project_Node_Id := Empty_Node;
      V_Name          : constant Name_Id := Get_String (Value);
      Ext_Var         : constant Name_Id := Get_String (External_Name);

      procedure Callback (Project, Parent, Node, Choice : Project_Node_Id);
      --  Called for each matching node for the env. variable

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

               if Next_Literal_String (C, Tree_N) = Empty_Node then
                  raise Delete_Variable;
               end if;

               if String_Value_Of (C, Tree_N) = V_Name then
                  Set_First_Literal_String
                    (Node, Tree_N, Next_Literal_String (C, Tree_N));
                  return;
               end if;

               loop
                  C2 := Next_Literal_String (C, Tree_N);
                  exit when C2 = Empty_Node;

                  if String_Value_Of (C2, Tree_N) = V_Name then
                     Set_Next_Literal_String
                       (C, Tree_N, Next_Literal_String (C2, Tree_N));
                     exit;
                  end if;
                  C := C2;
               end loop;

            when N_External_Value =>
               if External_Default_Of (Node, Tree_N) /= Empty_Node
                 and then String_Value_Of
                   (External_Default_Of (Node, Tree_N), Tree_N) = V_Name
               then
                  Set_External_Default_Of (Node, Tree_N, Empty_Node);
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
                  exit when C2 = Empty_Node;

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

      if Prj.Ext.Value_Of (Tree.Data.Env.Env.External, Ext_Var) = V_Name then
         if Type_Decl /= Empty_Node then
            Prj.Ext.Add (Tree.Data.Env.Env.External,
                 External_Name,
                 Get_String (String_Value_Of
                               (First_Literal_String (Type_Decl, Tree_N),
                                Tree_N)),
                 Prj.Ext.From_Command_Line);
         else
            Prj.Ext.Add (Tree.Data.Env.Env.External, External_Name, "",
                         Prj.Ext.From_Command_Line);
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
      Tree_N         : constant Project_Node_Tree_Ref := Tree.Data.Tree;
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

         if Var /= Empty_Node then
            Type_Node := String_Type_Of (Var, Tree_N);
            pragma Assert (Type_Node /= Empty_Node);
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
            Unchecked_Free (NS);
         end loop;

         Unchecked_Free (Self.Predefined_Object_Path);
         Unchecked_Free (Self.Predefined_Source_Path);
         Unchecked_Free (Self.Predefined_Project_Path);
         Unchecked_Free (Self.Predefined_Source_Files);
         Free (Self.Xrefs_Subdir);
         Self.Extensions.Clear;

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
            Prj.Tree.Tree_Private_Part.Project_Node_Table.Free
              (Self.Tree.Project_Nodes);
         end if;

         Free (Self.Tree);
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

begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;

   --  Disable verbose messages from project manager, not useful in GPS
   Opt.Quiet_Output := True;

   --  Unchecked_Shared_Lib_Imports is only relevant for builders
   Opt.Unchecked_Shared_Lib_Imports := True;
end GNATCOLL.Projects;
