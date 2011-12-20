------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

pragma Ada_05;

with Ada.Strings.Unbounded;
with GNATCOLL.Python;  use GNATCOLL.Python;

package GNATCOLL.Scripts.Python is

   Python_Name : constant String := "python";

   procedure Register_Python_Scripting
     (Repo         : access Scripts.Scripts_Repository_Record'Class;
      Module       : String;
      Program_Name : String := "python";
      Python_Home  : String := "");
   --  All commands and classes will be added in the specified module.
   --
   --  Program_Name should be the name of the program registering Python
   --  scripting. The interpreter will resove run-time libraries relative to
   --  this executable.
   --
   --  If Python_Home is non-empty, it will be used as home, and libraries will
   --  be searched for in <Python_Home>/lib/python<version>

   procedure Unregister_Python_Scripting
     (Repo : access Scripts.Scripts_Repository_Record'Class);
   --  Mark the python scripting language as no longer valid. This should be
   --  called before your application exits, to prevent unwanted storage_error
   --  in the finalization of the application (since some class_instances might
   --  be automatically finalized after python itself was destroyed, otherwise)

   type Python_Scripting_Record is new Scripting_Language_Record with private;
   type Python_Scripting is access all Python_Scripting_Record'Class;
   pragma No_Strict_Aliasing (Python_Scripting);

   type Python_Callback_Data is new Callback_Data with private;

   function Get_Param
     (Data : Python_Callback_Data'Class; N : Positive)
      return PyObject;
   procedure Get_Param
     (Data    : Python_Callback_Data'Class;
      N       : Positive;
      Result  : out PyObject;
      Success : out Boolean);
   --  Return the N-th command line parameter, taking into account the keywords
   --  if any.
   --  The returned value is a borrowed reference and must not be DECREF'd

private

   ----------------------
   -- Python_scripting --
   ----------------------

   type Python_Scripting_Record is new Scripting_Language_Record with record
      Repo                   : Scripts_Repository;
      Finalized              : Boolean := False;
      Blocked                : Boolean := False;
      Module                 : PyObject;
      Builtin                : PyObject;
      Exception_Misc         : PyObject;
      Exception_Missing_Args : PyObject;
      Exception_Invalid_Arg  : PyObject;
      Exception_Unexpected   : PyObject;

      Globals                : PyObject;
      --  The global symbols for the python interpreter

      Use_Secondary_Prompt   : Boolean := False;
      --  Which type of prompt should be displayed

      Buffer                 : GNAT.Strings.String_Access;
      --  Buffer for the command, to be added in front of any command before
      --  executing. This is used for multi-line input.

      Ignore_Constructor     : Boolean := False;
      --  Whether we are creating a new instance of a class.
      --  This is used to disable the call to __init__ (for backward
      --  compatibility and because we wouldn't know how to pass extra
      --  arguments to New_Instance).

      In_Process             : Boolean := False;
      --  True while we are processing a command. This is used to control the
      --  behavior of control-c: either interrupt, or copy.

      Current_File           : Ada.Strings.Unbounded.Unbounded_String;
      --  The script we are currently executing
   end record;

   overriding function Command_Line_Treatment
     (Script : access Python_Scripting_Record) return Command_Line_Mode;
   overriding procedure Destroy (Script : access Python_Scripting_Record);
   overriding procedure Block_Commands
     (Script : access Python_Scripting_Record; Block  : Boolean);
   overriding procedure Register_Command
     (Script : access Python_Scripting_Record;
      Cmd    : Command_Descr_Access);
   overriding procedure Register_Property
     (Script : access Python_Scripting_Record;
      Prop   : Property_Descr_Access);
   overriding procedure Register_Class
     (Script : access Python_Scripting_Record;
      Name   : String;
      Base   : Class_Type := No_Class);
   overriding function Create
     (Script          : access Python_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   overriding function New_Instance
     (Script : access Python_Scripting_Record;
      Class  : Class_Type) return Class_Instance;
   overriding procedure Execute_Command
     (Script       : access Python_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   overriding function Execute_Command
     (Script       : access Python_Scripting_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   overriding function Execute_Command
     (Script      : access Python_Scripting_Record;
      CL          : Arg_List;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean;
   overriding function Execute_Command
     (Script  : access Python_Scripting_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class;
      Error   : access Boolean) return String;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class;
      Error   : access Boolean) return Any_Type;
   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class;
      Error   : access Boolean) return PyObject;
   --  Need to unref the returned value

   function Execute_Command
     (Script  : access Python_Scripting_Record'Class;
      Command : PyObject;
      Args    : Callback_Data'Class;
      Error   : access Boolean) return Boolean;
   overriding procedure Load_Directory
     (Script    : access Python_Scripting_Record;
      Directory : GNATCOLL.VFS.Virtual_File;
      To_Load   : Script_Loader := Load_All'Access);
   overriding procedure Execute_File
     (Script       : access Python_Scripting_Record;
      Filename     : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   overriding function Get_Name
     (Script : access Python_Scripting_Record) return String;
   overriding function Get_Repository
     (Script : access Python_Scripting_Record) return Scripts_Repository;
   overriding function Current_Script
     (Script : access Python_Scripting_Record) return String;
   overriding procedure Set_Default_Console
     (Script  : access Python_Scripting_Record;
      Console : Virtual_Console);
   overriding procedure Display_Prompt
     (Script  : access Python_Scripting_Record;
      Console : Virtual_Console := null);
   overriding function Interrupt
     (Script : access Python_Scripting_Record) return Boolean;
   overriding procedure Complete
     (Script      : access Python_Scripting_Record;
      Input       : String;
      Completions : out String_Lists.List);
   overriding function New_List
     (Script : access Python_Scripting_Record;
      Class  : Class_Type := No_Class) return List_Instance'Class;
   --  See doc from inherited subprograms

   type Python_Callback_Data is new Callback_Data with record
      Script           : Python_Scripting;

      Args, Kw         : PyObject;
      --  Args is a tuple, a list, or any iterable.
      --  These are the arguments passed by python. If Name_Parameters was
      --  called, these are modified in place: Kw is reset to null, and its
      --  contents merged into Args. Args is resized appropriately (to the
      --  number of arguments passed to Name_Parameters). This cannot be used
      --  for functions with a variable number of parameters.

      Return_Value      : PyObject;
      Return_Dict       : PyObject;
      Has_Return_Value  : Boolean := False;
      Return_As_List    : Boolean := False;

      First_Arg_Is_Self : Boolean;
      --  True if the first argument is "self", ie we are calling a method
   end record;

   overriding function Clone
     (Data : Python_Callback_Data) return Callback_Data'Class;
   overriding function Get_Script
     (Data : Python_Callback_Data) return Scripting_Language;
   overriding function Number_Of_Arguments
     (Data : Python_Callback_Data) return Natural;
   overriding procedure Name_Parameters
     (Data : in out Python_Callback_Data; Names : Cst_Argument_List);
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return String;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Unbounded_String;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Integer;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Boolean;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Class : Class_Type;
      Allow_Null : Boolean := False)
      return Class_Instance;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : String)
      return String;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : Integer)
      return Integer;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive; Default : Boolean)
      return Boolean;
   overriding function Nth_Arg
     (Data       : Python_Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Default    : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance;
   overriding function Nth_Arg
     (Data    : Python_Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type;
   overriding function Nth_Arg
     (Data : Python_Callback_Data; N : Positive) return List_Instance'Class;
   overriding procedure Set_Error_Msg
     (Data : in out Python_Callback_Data; Msg : String);
   overriding procedure Set_Return_Value_As_List
     (Data  : in out Python_Callback_Data;
      Size  : Natural := 0;
      Class : Class_Type := No_Class);
   overriding procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Integer);
   overriding procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : String);
   overriding procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Boolean);
   overriding procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : Class_Instance);
   overriding procedure Set_Return_Value
     (Data : in out Python_Callback_Data; Value : List_Instance);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : String;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Integer;
      Append : Boolean := False);
   overriding procedure Set_Return_Value_Key
     (Data   : in out Python_Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False);
   overriding procedure Free (Data : in out Python_Callback_Data);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : String);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : Integer);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data; N : Positive; Value : Boolean);
   overriding procedure Set_Nth_Arg
     (Data : in out Python_Callback_Data;
      N : Positive; Value : Class_Instance);
   overriding procedure Set_Nth_Arg
     (Data  : in out Python_Callback_Data;
      N     : Positive;
      Value : List_Instance);
   overriding procedure Set_Nth_Arg
     (Data  : in out Python_Callback_Data;
      N     : Positive;
      Value : Subprogram_Type);
   overriding procedure Execute_Command
     (Args    : in out Python_Callback_Data;
      Command : String);
   overriding function Return_Value
     (Data : Python_Callback_Data) return String;
   overriding function Return_Value
     (Data : Python_Callback_Data) return Integer;
   overriding function Return_Value
     (Data : Python_Callback_Data) return Boolean;
   overriding function Return_Value
     (Data : Python_Callback_Data) return Class_Instance;
   overriding function Return_Value
     (Data : Python_Callback_Data) return List_Instance'Class;
   --  See doc from inherited subprogram

end GNATCOLL.Scripts.Python;
