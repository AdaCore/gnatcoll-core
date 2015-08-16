------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

--  This internal unit provides support for editing projects.

--  Project files can be written freely by the user (through any standard
--  editor). However, although we are able to import them whatever form they
--  have, these can'be easily manipulated, and a different form needs to be
--  used, called normalized project files.
--
--  Projects are normalized only the first time they are actually modified (ie
--  if they are open in the project browser but never modified, then we don't
--  need to modify what the user did, since Prj.Proc.Process can of course work
--  with any form of projects).
--
--  However, the normalized projects are needed, so that we know exactly where
--  to add new statements depending on the current scenario.
--
--  Normalized projects have the following invariant:
--      There is only one case statement per project or package.
--  This is in fact a nested case statement, where each environment variable is
--  referenced.
--
--  They also have the following invariant:
--      A project has exactly the same behavior in its normalized form as in
--      its original form.
--  Of course, this is only true until the next modification to any of the two
--  forms.
--
--  Thus, the projects have the following format:
--
--      Project_Header
--      [Variable_Declarations]
--      [Common_Section]
--      [Nested_Case]
--      [Package_Declaration
--         [Common_Section]
--         [Nested_Case]
--      ]*
--
--  Where:
--     Project_Header is the standard header, including importing other
--     projects, declaring the name of the current project, ...
--
--     Variable_Declarations is the list of scenario variables, including their
--     types. There can be no variable declaration outside of this section,
--     including in packages.
--     ??? Not two variables can reference the same external variables.
--
--     Common_Section is the list of statements that need to be executed in all
--     scenarios (like common source directories, common switches when inside a
--     package, ...). This section can not include any case statement.
--
--     Nested_Case is one big case statement, including other nested cases. Its
--     format is similar to:
--
--           case Var1 is
--              when Value1 =>
--                 case Var2 is
--                    when Value1_1 => stmt1;
--                    when Value1_2 => stmt2;
--                 end case;
--              when Value2 =>
--                 case Var2 is
--                    when Value2_1 => stmt3;
--                    when Value2_2 => stmt4;
--                 end case;
--           end case;
--
--     The "when others" section is not allowed in the nested cases, and are
--     replaced by the appropriate list of "when" statements.

private package GNATCOLL.Projects.Normalize is

   Normalize_Error : exception;
   --  Raised when a project could not be normalized (necessary step before any
   --  modification). The project cannot be edited by GPS.
   --  Any subprogram in this package might raise this exception. In that case,
   --  the exception message is set to the text of the error.

   procedure Normalize
     (Tree    : Project_Tree_Data_Access;
      Project : Project_Type);
   --  Normalize Project.
   --  The exception Normalize_Error is raised if Project uses some features
   --  that cannot currently be normalized.
   --  If Recurse is true, then imported projects area also normalized.

   function Get_String (Id  : Namet.Name_Id) return String;
   function Get_String (Str : String)        return Namet.Name_Id;

   function Clone_Node
     (Tree       : Prj.Tree.Project_Node_Tree_Ref;
      Node       : Prj.Tree.Project_Node_Id;
      Deep_Clone : Boolean := False)
      return Prj.Tree.Project_Node_Id;
   --  Return a copy of Node. If Deep_Clone is true, then all the children of
   --  node are also copied.
   --  If Deep_Clone is false, then the two nodes will share part of their
   --  structure.
   --
   --  Note: nodes like variable or type declarations, packages,... are not
   --  chained up when they are cloned, you need to recreate the proper lists
   --  afterwards. See Post_Process_After_Clone below
   --
   --  A special case also occurs for a N_Typed_Variable_Declaration, since the
   --  type that is referenced is a pointer to the same node as the type for
   --  Node. No deep copy is done for this type. This needs to be fixed in a
   --  post-processing phase, as above.
   --
   --  The same limitation exists for N_Variable_Reference and
   --  N_Attribute_Reference and the package they are referencing

   function Is_Virtual_Extending
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Node : Prj.Tree.Project_Node_Id) return Boolean;
   --  Return True if Node is a virtual extending project created
   --  automatically by GNAT's project manager

   function Find_Type_Declaration
     (Tree    : Prj.Tree.Project_Node_Tree_Ref;
      Project : Prj.Tree.Project_Node_Id;
      Name    : Namet.Name_Id) return Prj.Tree.Project_Node_Id;
   --  Return the declaration of the type whose name is Name

   function Create_Typed_Variable
     (Tree                         : Prj.Tree.Project_Node_Tree_Ref;
      Prj_Or_Pkg                   : Prj.Tree.Project_Node_Id;
      Name                         : String;
      Typ                          : Prj.Tree.Project_Node_Id;
      Add_Before_First_Case_Or_Pkg : Boolean := False)
      return Prj.Tree.Project_Node_Id;
   --  Create a new variable of a specific type Typ.
   --  The declaration is appended at the end of the declarative items list in
   --  the project or the package, unless Add_Before_First_Case is True. In
   --  this case, it is put just before the first N_Case_Construction node is
   --  encountered (i.e the last position in the common section of a normalized
   --  project).

   procedure Add_In_Front
     (Tree   : Prj.Tree.Project_Node_Tree_Ref;
      Parent : Prj.Tree.Project_Node_Id;
      Node   : Prj.Tree.Project_Node_Id);
   --  Add Node at the begining of the list for Parent.
   --  Node can also be a N_Declarative_Item (or a list of them).

   procedure Normalize_Cases
     (Tree    : Prj.Tree.Project_Node_Tree_Ref;
      Project : Project_Type);
   --  Make sure that all possible values of a variable appear in a case
   --  statement, to avoid warnings from the project manager.
   --  This subprogram doesn't apply recursively to imported projects

   function Add_Imported_Project
     (Tree                      : Project_Tree_Data_Access;
      Project                   : Project_Type'Class;
      Imported_Project          : Project_Type'Class;
      Errors                    : Error_Report := null;
      Use_Relative_Path         : Boolean;
      Use_Base_Name             : Boolean;
      Limited_With              : Boolean := False)
      return Import_Project_Error;
   --  Internal version of Add_Imported_Project.
   --  You must have computed the list of importing projects for Project.

   ---------------
   -- Variables --
   ---------------

   function Is_External_Variable
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Prj.Tree.Project_Node_Tree_Ref) return Boolean;
   --  Return True if Var is a reference to an external variable

   function External_Reference_Of
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Prj.Tree.Project_Node_Tree_Ref) return Namet.Name_Id;
   --  Return the name of the external reference used in the declaration of
   --  Var (Var := external ("REF")).

   procedure Set_Value_As_External
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Var           : Prj.Tree.Project_Node_Id;
      External_Name : String;
      Default       : String := "");
   --  Set the value of the variable as a reference to the environment variable
   --  External_Name. Var must be a single value, not a string.
   --  If Var is a typed variable, the default value is checked against the
   --  list of possible values (Invalid_Value raised if not).

   function Create_Variable_Reference
     (Tree : Prj.Tree.Project_Node_Tree_Ref;
      Var  : Prj.Tree.Project_Node_Id)
      return Prj.Tree.Project_Node_Id;
   --  Create and return a reference to the variable Var.
   --  Var must be a variable declaration

   procedure Delete_Scenario_Variable
     (Tree                     : Project_Tree_Data_Access;
      Root_Project             : Project_Type;
      External_Name            : String;
      Keep_Choice              : String;
      Delete_Direct_References : Boolean := True);
   --  Internal version of Delete_External_Variable

   function Create_Type
     (Tree       : Prj.Tree.Project_Node_Tree_Ref;
      Prj_Or_Pkg : Prj.Tree.Project_Node_Id;
      Name       : String) return Prj.Tree.Project_Node_Id;
   --  Create a new type. By default, there is no possible value, you
   --  must add some with Add_Possible_Value.
   --  The new declaration is added at the end of the declarative item list for
   --  Prj_Or_Pkg (but before any package declaration).

   procedure Add_Possible_Value
     (Tree   : Prj.Tree.Project_Node_Tree_Ref;
      Typ    : Prj.Tree.Project_Node_Id;
      Choice : String);
   --  Add a new choice in the list of possible values for the type Typ.
   --  If Choice is already available in Typ, then it is not added again.

   function Expression_As_String
     (Tree       : Prj.Tree.Project_Node_Tree_Ref;
      Expression : Prj.Tree.Project_Node_Id) return Namet.Name_Id;
   --  Return the string contained in an expression. If the expression contains
   --  more than a string literal, No_Name is returned.
   --  This also accepts cases when Expression itself is a string_literal

   function Find_Scenario_Variable
     (Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Project       : Project_Type;
      External_Name : String) return Prj.Tree.Project_Node_Id;
   --  Return the declaration of the scenario variable associated with
   --  the external variable External_Name.
   --  In normalized projects, there should be only such variable.

   type Environment_Variable_Callback is access procedure
     (Project, Parent, Node, Choice : Prj.Tree.Project_Node_Id);
   --  Callback for For_Each_Environment_Variable.
   --  The various possible combinations are:
   --    Node                    Parent                 Choice
   --  N_Variable_Reference      N_Term in expression   Empty_Node
   --  N_External_Value          N_Term in expression   Empty_Node
   --  N_Case_Item               N_Declarative_Item of  matching choice
   --                            case construction      N_Literal_String
   --  N_String_Type_Declaration Empty_Node             Empty_Node
   --  N_Typed_Variable_Declaration N_Declarative_Item  Empty_Node

   procedure For_Each_Environment_Variable
     (Tree              : Prj.Tree.Project_Node_Tree_Ref;
      Root_Project      : Project_Type;
      Ext_Variable_Name : Namet.Name_Id;
      Specific_Choice   : Namet.Name_Id;
      Action            : Environment_Variable_Callback);
   --  Iterate over all possible references to an external variable. This
   --  returns N_External_Value, N_Variable_Reference,
   --  N_Typed_Variable_Declaration and N_String_Type_Declaration (the last
   --  three are indirect references through a named variable.

   -------------
   -- Editing --
   -------------

   procedure Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : Attribute_Pkg_String;
      Value     : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      At_Index  : Natural := 0);
   procedure Set_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : Attribute_Pkg_List;
      Values    : GNAT.Strings.String_List;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "";
      Prepend   : Boolean := False);
   --  Edit the value of an attribute

   procedure Delete_Attribute
     (Tree      : Project_Tree_Data_Access;
      Project   : Project_Type;
      Attribute : String;
      Scenario  : Scenario_Variable_Array := All_Scenarios;
      Index     : String := "");
   --  Delete an attribute from the project

   procedure Rename_And_Move
     (Tree          : Project_Tree_Data_Access;
      Project       : Project_Type;
      New_Name      : String;
      New_Path      : GNATCOLL.VFS.Virtual_File;
      Errors        : Error_Report := null);
   --  Internal version of Rename_And_Move

   function Rename_Path
     (Tree               : Project_Tree_Data_Access;
      Project            : Project_Type;
      Old_Path           : GNATCOLL.VFS.Virtual_File;
      New_Path           : GNATCOLL.VFS.Virtual_File;
      Use_Relative_Paths : Boolean) return Boolean;
   --  Internal version of Rename_Path

end GNATCOLL.Projects.Normalize;
