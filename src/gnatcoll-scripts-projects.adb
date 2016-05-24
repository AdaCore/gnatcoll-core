------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.Scripts.Files;

package body GNATCOLL.Scripts.Projects is

   type Project_Properties_Record is new Instance_Property_Record with record
      Project : Project_Type;
   end record;

   Project_Class_Name : constant String := "Project";

   Name_Cst           : aliased constant String := "name";
   Recursive_Cst      : aliased constant String := "recursive";
   Attribute_Cst      : aliased constant String := "attribute";
   Package_Cst        : aliased constant String := "package";
   Prefix_Cst         : aliased constant String := "prefix";
   Index_Cst          : aliased constant String := "index";

   Sources_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Recursive_Cst'Access);
   Source_Dirs_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Recursive_Cst'Access);
   Languages_Cmd_Parameters : constant GNATCOLL.Scripts.Cst_Argument_List :=
     (1 => Recursive_Cst'Access);

   Project_Cmd_Parameters   : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access);
   Get_Attributes_Parameters : constant Cst_Argument_List :=
     (1 => Attribute_Cst'Unchecked_Access,
      2 => Package_Cst'Unchecked_Access,
      3 => Index_Cst'Unchecked_Access);

   Scenar_Var_Parameters    : constant Cst_Argument_List :=
                                (1 => Prefix_Cst'Access);

   type Project_Tree_Retriever_Access is
     access all Project_Tree_Retriever'Class;

   Retriever : Project_Tree_Retriever_Access;

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);

   procedure Project_Queries
     (Data : in out Callback_Data'Class; Command : String);

   procedure Set_Data
     (Instance : Class_Instance; Project  : Project_Type);

   function Scenario_Variables_Cmd_Line (Prefix : String) return String;
   --  Return the command line to use to set up the scenario variables when
   --  calling an external tool that handles project files.
   --  For a Makefile, set Prefix to "", for gnatmake set prefix to "-X".
   --  This function returns a concatenation of Prefix & "VAR=VALUE".

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : GNATCOLL.Projects.Project_Type)
      return Class_Instance
   is
      Instance : Class_Instance := No_Class_Instance;
   begin
      if Project /= No_Project then
         Instance := New_Instance
           (Script, New_Class (Get_Repository (Script), Project_Class_Name));
         Set_Data (Instance, Project);
      end if;

      return Instance;
   end Create_Project;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class;
      N : Positive)
      return GNATCOLL.Projects.Project_Type
   is
      Class : constant Class_Type := Get_Project_Class (Get_Repository (Data));
      Inst  : constant Class_Instance :=
        Nth_Arg (Data, N, Class, Allow_Null => True);
      Value : Instance_Property;

   begin
      if Inst = No_Class_Instance then
         return No_Project;
      end if;

      Value := Get_Data (Inst, Project_Class_Name);
      if Value = null then
         return No_Project;
      else
         return Project_Properties_Record (Value.all).Project;
      end if;
   end Get_Data;

   -----------------------
   -- Get_Project_Class --
   -----------------------

   function Get_Project_Class
     (Repo : access Scripts_Repository_Record'Class)
      return Class_Type is
   begin
      return New_Class (Repo, Project_Class_Name);
   end Get_Project_Class;

   -----------------------------
   -- Project_Command_Handler --
   -----------------------------

   procedure Project_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Repo   : constant Scripts_Repository := Get_Repository (Data);
      Instance : Class_Instance;
      Project  : Project_Type;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Project_Cmd_Parameters);
         Project  := Project_Tree.Project_From_Name
           (Nth_Arg (Data, 2));

         if Project = No_Project then
            Set_Error_Msg (Data, "No such project: " & Nth_Arg (Data, 2));
         else
            Instance := Nth_Arg (Data, 1, Get_Project_Class (Repo));
            Set_Data (Instance, Project);
         end if;

      elsif Command = "root" then
         Set_Return_Value
           (Data, Create_Project (Get_Script (Data),
                                  Project_Tree.Root_Project));

      elsif Command = "name" then
         Project := Get_Data (Data, 1);
         Set_Return_Value (Data, Project.Name);

      elsif Command = "file" then
         Project := Get_Data (Data, 1);
         Set_Return_Value
           (Data,
            GNATCOLL.Scripts.Files.Create_File
              (Get_Script (Data), Project_Path (Project)));

      elsif Command = "ancestor_deps" then
         declare
            Iter : Project_Iterator;
            P    : Project_Type;
         begin
            Project := Get_Data (Data, 1);
            Set_Return_Value_As_List (Data);
            Iter := Find_All_Projects_Importing
              (Project, Include_Self => True);

            loop
               P := Current (Iter);
               exit when P = No_Project;
               Set_Return_Value
                 (Data, Create_Project (Get_Script (Data), P));
               Next (Iter);
            end loop;
         end;

      elsif Command = "dependencies" then
         Name_Parameters (Data, (1 => Recursive_Cst'Access));
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Iter : Project_Iterator;
            P    : Project_Type;
         begin
            Project := Get_Data (Data, 1);
            Set_Return_Value_As_List (Data);
            Iter := Start
              (Project, Recursive => True, Direct_Only => not Recursive);

            loop
               P := Current (Iter);
               exit when P = No_Project;
               Set_Return_Value
                 (Data, Create_Project (Get_Script (Data), P));
               Next (Iter);
            end loop;
         end;

      elsif Command = "get_attribute_as_list" then
         Name_Parameters (Data, Get_Attributes_Parameters);
         declare
            Project : constant Project_Type := Get_Data (Data, 1);
            Attr    : constant String := Nth_Arg (Data, 2);
            Pkg     : constant String := Nth_Arg (Data, 3, "");
            Index   : constant String := Nth_Arg (Data, 4, "");
            List    : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
            Value   : constant String := Project.Attribute_Value
                   (Attribute_Pkg_String'(Build (Pkg, Attr)),
                   Default => "", Index => Index, Use_Extended => True);
         begin
            Set_Return_Value_As_List (Data);

            if List = null and then Value /= "" then
               Set_Return_Value (Data, Value);
            elsif List /= null then
               for L in List'Range loop
                  Set_Return_Value (Data, List (L).all);
               end loop;
            end if;

            Free (List);
         end;

      elsif Command = "get_attribute_as_string" then
         Name_Parameters (Data, Get_Attributes_Parameters);
         declare
            Project : constant Project_Type := Get_Data (Data, 1);
            Attr    : constant String := Nth_Arg (Data, 2);
            Pkg     : constant String := Nth_Arg (Data, 3, "");
            Index   : constant String := Nth_Arg (Data, 4, "");
            Value   : constant String := Project.Attribute_Value
                   (Attribute_Pkg_String'(Build (Pkg, Attr)),
                   Default => "", Index => Index, Use_Extended => True);
         begin
            if Value = "" then
               declare
                  Result : Unbounded_String;
                  List   : String_List_Access := Project.Attribute_Value
                    (Attribute_Pkg_List'(Build (Pkg, Attr)), Index);
               begin
                  if List /= null then
                     for L in List'Range loop
                        Append (Result, List (L).all);

                        if L /= List'Last then
                           Append (Result, " ");
                        end if;
                     end loop;

                     Free (List);
                  end if;

                  Set_Return_Value (Data, To_String (Result));
               end;
            else
               Set_Return_Value (Data, Value);
            end if;
         end;

      elsif Command = "scenario_variables" then
         declare
            Vars : constant Scenario_Variable_Array :=
              Project_Tree.Scenario_Variables;
         begin
            for V in Vars'Range loop
               Set_Return_Value (Data, Value (Vars (V)));
               Set_Return_Value_Key
                 (Data, External_Name (Vars (V)));
            end loop;
         end;

      elsif Command = "scenario_variables_cmd_line" then
         Name_Parameters (Data, Scenar_Var_Parameters);
         declare
            Prefix : constant String := Nth_Arg (Data, 1, "");
         begin
            Set_Return_Value (Data, Scenario_Variables_Cmd_Line (Prefix));
         end;

      elsif Command = "scenario_variables_values" then
         declare
            Tree : constant Project_Tree_Access := Project_Tree;
            Vars : constant Scenario_Variable_Array := Tree.Scenario_Variables;
         begin
            for V in Vars'Range loop
               declare
                  Name   : constant String := External_Name (Vars (V));
                  Values : String_List := Tree.Possible_Values_Of (Vars (V));
               begin
                  for Iter in Values'Range loop
                     Set_Return_Value (Data, Values (Iter).all);
                     Set_Return_Value_Key (Data, Name, True);
                  end loop;

                  Free (Values);
               end;
            end loop;
         end;

      end if;
   end Project_Command_Handler;

   ---------------------
   -- Project_Queries --
   ---------------------

   procedure Project_Queries
     (Data : in out Callback_Data'Class; Command : String)
   is

      Project : constant Project_Type := Get_Data (Data, 1);

   begin
      if Command = "get_executable_name" then
         declare
            Main : constant Virtual_File :=
              GNATCOLL.Scripts.Files.Nth_Arg (Data, 2);
         begin
            Set_Return_Value
              (Data, Project.Executable_Name (Main.Full_Name.all));
         end;

      elsif Command = "sources" then
         Name_Parameters (Data, Sources_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Sources   : File_Array_Access := Project.Source_Files
              (Recursive  => Recursive);
         begin
            Set_Return_Value_As_List (Data);
            for S in Sources'Range loop
               Set_Return_Value
                 (Data, GNATCOLL.Scripts.Files.Create_File
                    (Get_Script (Data), Sources (S)));
            end loop;
            Unchecked_Free (Sources);
         end;

      elsif Command = "languages" then
         Name_Parameters (Data, Languages_Cmd_Parameters);
         declare
            Langs : GNAT.Strings.String_List := Project.Languages
              (Recursive => Nth_Arg (Data, 2, False));
         begin
            Set_Return_Value_As_List (Data);
            for L in Langs'Range loop
               Set_Return_Value (Data, To_Lower (Langs (L).all));
            end loop;
            Free (Langs);
         end;

      elsif Command = "source_dirs" then
         Name_Parameters (Data, Source_Dirs_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Dirs      : constant File_Array := Project.Source_Dirs
              (Recursive => Recursive);
         begin
            Set_Return_Value_As_List (Data);

            for D in Dirs'Range loop
               --  ??? We should return the Virtual_File object instead
               Set_Return_Value (Data, Dirs (D).Full_Name);
            end loop;
         end;

      elsif Command = "object_dirs" then
         Name_Parameters (Data, Source_Dirs_Cmd_Parameters);
         declare
            Recursive : constant Boolean := Nth_Arg (Data, 2, False);
            Object    : constant File_Array := Object_Path
              (Project,
               Recursive           => Recursive,
               Including_Libraries => False);
         begin
            Set_Return_Value_As_List (Data);

            for J in Object'Range loop
               --  ??? Shouldn't we return a list of files instead ?
               Set_Return_Value (Data, Object (J).Full_Name);
            end loop;
         end;
      elsif Command = "exec_dir" then
         declare
            Exec_Dir : constant Virtual_File := Project.Executables_Directory;
         begin
            Set_Return_Value (Data, Exec_Dir.Full_Name);
         end;
      end if;
   end Project_Queries;

   ------------------
   -- Project_Tree --
   ------------------

   function Project_Tree return GNATCOLL.Projects.Project_Tree_Access is
   begin
      if Retriever = null then
         return null;
      else
         return Retriever.Get_Project_Tree;
      end if;
   end Project_Tree;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Repo  : not null access Scripts_Repository_Record'Class;
      Value : not null access Project_Tree_Retriever'Class)
   is
   begin
      Retriever := Project_Tree_Retriever_Access (Value);

      Register_Command
        (Repo, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "root",
         Class         => Get_Project_Class (Repo),
         Static_Method => True,
         Handler       => Project_Command_Handler'Access);
      Register_Command
        (Repo, "name",
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "file",
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "ancestor_deps",
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "dependencies",
         Class        => Get_Project_Class (Repo),
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "get_attribute_as_string",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "get_attribute_as_list",
         Minimum_Args => 1,
         Maximum_Args => 3,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Command_Handler'Access);
      Register_Command
        (Repo, "scenario_variables",
         Class         => Get_Project_Class (Repo),
         Static_Method => True,
         Handler       => Project_Command_Handler'Access);
      Register_Command
        (Repo, "scenario_variables_cmd_line",
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Get_Project_Class (Repo),
         Static_Method => True,
         Handler       => Project_Command_Handler'Access);
      Register_Command
        (Repo, "scenario_variables_values",
         Minimum_Args  => 0,
         Maximum_Args  => 0,
         Class         => Get_Project_Class (Repo),
         Static_Method => True,
         Handler       => Project_Command_Handler'Access);

      Register_Command
        (Repo, "sources",
         Maximum_Args => Sources_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Queries'Access);
      Register_Command
        (Repo, "source_dirs",
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Queries'Access);
      Register_Command
        (Repo, "get_executable_name",
         Params  => (1 => Param ("main")),
         Class   => Get_Project_Class (Repo),
         Handler => Project_Queries'Access);
      Register_Command
        (Repo, "languages",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Queries'Access);
      Register_Command
        (Repo, "object_dirs",
         Minimum_Args => Source_Dirs_Cmd_Parameters'Length - 1,
         Maximum_Args => Source_Dirs_Cmd_Parameters'Length,
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Queries'Access);
      Register_Command
        (Repo, "exec_dir",
         Class        => Get_Project_Class (Repo),
         Handler      => Project_Queries'Access);
   end Register_Commands;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line (Prefix : String) return String is
      Scenario_Vars : constant Scenario_Variable_Array :=
        Project_Tree.Scenario_Variables;

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the result.

      ------------
      -- Concat --
      ------------

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String is
      begin
         if Index > Scenario_Vars'Last then
            return Current;
         end if;

         return Concat
           (Current
            & Set_Var & External_Name (Scenario_Vars (Index))
            & "=" & Value (Scenario_Vars (Index))
            & " ",
            Index + 1,
            Set_Var);
      end Concat;

   begin
      --  A recursive function is probably not the most efficient way, but this
      --  prevents limits on the command line lengths. This also avoids the use
      --  of unbounded strings.
      return Concat ("", Scenario_Vars'First, Prefix);
   end Scenario_Variables_Cmd_Line;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Project  : Project_Type) is
   begin
      if not Is_Subclass (Instance, Project_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, Project_Class_Name,
         Project_Properties_Record'(Project => Project));
   end Set_Data;

end GNATCOLL.Scripts.Projects;
