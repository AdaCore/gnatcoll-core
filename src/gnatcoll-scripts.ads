------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

--  This module provides various types and subprograms to integrate various
--  external scripting languages.
--  This API was designed so that multiple scripting languages can be used with
--  your application, and so that the core of the applicatoin and all the
--  various modules remain as independant as possible from the specific
--  language.

pragma Ada_05;

with Ada.Calendar;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.Arg_Lists;     use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;         use GNATCOLL.Utils;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GNATCOLL.Any_Types;     use GNATCOLL.Any_Types;

private with Interfaces;

package GNATCOLL.Scripts is

   type Scripts_Repository_Record is tagged private;
   type Scripts_Repository is access all Scripts_Repository_Record'Class;

   type Scripting_Language_Record is abstract tagged private;
   type Scripting_Language is access all Scripting_Language_Record'Class;

   type Cst_Argument_List is array (Natural range <>) of Cst_String_Access;

   type Callback_Data is abstract tagged private;
   type Callback_Data_Access is access all Callback_Data'Class;
   --  Data used to communicate with the scripting language engine, to marshall
   --  the parameters and return values.

   type Class_Instance (Initialized : Boolean := False) is private;
   --  ??? The discriminant declaration could be moved to the
   --  private declaration but is moved here to work around a bug in
   --  older versions of GNAT (J629-029)

   ----------------------
   -- Subprogram types --
   ----------------------

   type Subprogram_Record is abstract tagged private;
   type Subprogram_Type is access all Subprogram_Record'Class;
   pragma No_Strict_Aliasing (Subprogram_Type);
   --  This type represents a subprogram for the language. In Python, this
   --  is a python object which is a function or method.
   --  Do not confuse this with a shell command, it has a more general meaning.
   --  In particular, the user cannot define new shell commands in the GPS
   --  shell, and thus Subprogram_Record has a broader meaning.

   procedure Free (Subprogram : in out Subprogram_Type);
   --  Free the subprogram

   function Get_Script
     (Subprogram : Subprogram_Record) return Scripting_Language is abstract;
   --  Return the language in which the subprogram was written

   procedure Free (Subprogram : in out Subprogram_Record) is abstract;
   --  Free the memory occupied by the subprogram instance

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Boolean;
   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return String;
   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Class_Instance;
   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Any_Type;
   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class)
      return GNAT.Strings.String_List;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Boolean is abstract;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return String is abstract;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Class_Instance is abstract;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return Any_Type is abstract;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean)
      return GNAT.Strings.String_List is abstract;
   --  Execute the subprogram with the given arguments, and return its output.
   --  Returned value must be freed by the caller.
   --  For a String_List, some items in the result value might be left to null
   --  if the corresponding element from the shell is not a string.

   function Get_Name
     (Subprogram : access Subprogram_Record) return String is abstract;
   --  Return the name of the subprogram, as a string that can be displayed for
   --  the user. This is used when analysing the contents of a hook for
   --  instance

   -----------------
   -- Class types --
   -----------------

   type Class_Type is private;
   No_Class : constant Class_Type;
   --  A class type, which can be used to create new instances. Primitive
   --  operations (aka methods) can be associated with the class. This is the
   --  primary way to make new subprograms available to the user, while
   --  organizing them into namespaces.

   Any_Class : constant Class_Type;
   --  Constant that can be used in the call to Nth_Arg below to indicate
   --  that the nth parameter is an instance, but its actual class is
   --  undefined

   No_Class_Instance : constant Class_Instance;
   --  The instance of a class, which embeds some Ada data. This type is
   --  reference counted, and will automatically take care of memory management
   --  issues.

   function Lookup_Class
     (Repo : access Scripts_Repository_Record;
      Name : String) return Class_Type;
   --  Return a Class_Type for Name.
   --  If the given class does not exist, a dummy version is created (but is
   --  not exported to the scripting languages). This is for instance
   --  convenient to represent one of the builtin classes for the languages,
   --  although it might be dangerous since not all languages have the same
   --  builtins.
   --  If you use a dummy version as a base class in New_Class, and it doesn't
   --  exist in the language, then this is equivalent to not having a base
   --  class.

   function New_Class
     (Repo : access Scripts_Repository_Record'Class;
      Name : String;
      Base : Class_Type := No_Class) return Class_Type;
   --  For some languages, this notion is not supported, and the class will not
   --  be visible by the user in the shell. Methods created for the class will
   --  then simply be made available directly in the shell.
   --  If a class with the same name was created, it is returned, and no class
   --  is created anew.
   --  Base is the base class, or parent class. It only needs to be specified
   --  the first time the class is created (typically just before the matching
   --  calls to Register_Command), and can be left to its default value
   --  afterward.
   --  Description of the new class must be put in the file shell_commands.xml,
   --  which is read dynamically when generating the documentation.

   function Get_Name (Class : Class_Type) return String;
   --  Return the name of the class

   -------------------
   -- Callback_Data --
   -------------------

   Invalid_Parameter : exception;
   No_Such_Parameter : exception;

   function Create
     (Script          : access Scripting_Language_Record;
      Arguments_Count : Natural) return Callback_Data'Class is abstract;
   --  Create a new empty list of arguments. You must call Set_Nth_Arg for
   --  each of these arguments before using the return value.

   function Command_Line_Treatment
     (Script : access Scripting_Language_Record)
      return Command_Line_Mode is abstract;
   --  Indicates how command lines should be treated by GPS.
   --  If the returned type is Separate_Args, then GPS should handle the
   --  parsing and separating of arguments.
   --  Otherwise GPS should just manipulate the command lines as raw strings.

   procedure Free (Data : in out Callback_Data) is abstract;
   procedure Free (Data : in out Callback_Data_Access);
   --  Free the memory occupied by Data. This needs to be called only if Data
   --  was created through Create

   function Clone (Data : Callback_Data) return Callback_Data'Class
      is abstract;
   --  Clone Data. The result value must be freed by the caller

   procedure Set_Nth_Arg
     (Data : in out Callback_Data; N : Positive; Value : String) is abstract;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data; N : Positive; Value : Integer) is abstract;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data; N : Positive; Value : Float) is abstract;
   procedure Set_Nth_Arg
     (Data : in out Callback_Data; N : Positive; Value : Boolean) is abstract;
   procedure Set_Nth_Arg
     (Data  : in out Callback_Data;
      N     : Positive;
      Value : Class_Instance) is abstract;
   procedure Set_Nth_Arg
     (Data  : in out Callback_Data;
      N     : Positive;
      Value : Subprogram_Type) is abstract;
   procedure Set_Nth_Arg
     (Data  : in out Callback_Data'Class;
      N     : Positive;
      Value : Filesystem_String);
   --  Set the nth argument of Data

   function Number_Of_Arguments
     (Data : Callback_Data) return Natural is abstract;
   --  Return the number of arguments passed to that callback. The number of
   --  arguments has already been check before the transfer to your own
   --  subprogram.

   procedure Name_Parameters
     (Data : in out Callback_Data; Names : Cst_Argument_List) is abstract;
   --  Name the parameters, for languages which support it.
   --  For instance, the following call:
   --     Name_Parameters (Data, (1 => new String'("a"),
   --                             2 => new String'("b"),
   --                             3 => new String'("c")));
   --  will provide support for the following python calls:
   --     func (1, 2, 3)
   --     func (1, c=3, b=2)
   --  This call has no effect for languages which do not support name
   --  parameters.
   --  After calling this procedure, the parameters are reordered so that no
   --  matter what order the user specified them in, calling Nth_Arg (2) will
   --  always return the value for b.
   --  You should pass a default value to Nth_Arg, since otherwise if a
   --  parameter was not given on the command line, even if later parameters
   --  were given, Nth_Arg will raise Invalid_Parameter.
   --
   --  It is recommended that Names be a global constant, which you can also
   --  use when registering the command, through Parameter_Names_To_Usage, so
   --  that the documentation remains up-to-date.
   --
   --  Names should not include "self" in the case of methods. This is an
   --  implicit parameter in most languages.

   function Get_Script (Data : Callback_Data) return Scripting_Language
      is abstract;
   --  Return the scripting language that created Data

   function Get_Repository (Data : Callback_Data) return Scripts_Repository;
   --  Return the kernel associated with Data

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return String is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Unbounded_String is abstract;
   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive) return Filesystem_String;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Integer is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Float is abstract;
   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Boolean is abstract;
   --  Get the nth argument to the function, starting from 1.
   --  If there is not enough parameters, No_Such_Parameter is raised
   --  If the parameters doesn't have the right type, Invalid_Parameter is
   --  raised.

   function Nth_Arg
     (Data : Callback_Data; N : Positive) return Subprogram_Type is abstract;
   --  Same as above, for a subprogram. The returned value must be freed

   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Allow_Null : Boolean := False) return Class_Instance is abstract;
   --  The class_instance must belong to Class or its children, or
   --  Invalid_Parameter is also raised.
   --  The return value must be freed by the caller.
   --  If Allow_Null is true, then a null instance might be passed as a
   --  parameter. If it is false, passing a null instance will raise
   --  Invalid_Parameter.
   --  Class can be set to Any_Class to indicate that the instance can be
   --  of any class.

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : String) return String;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Filesystem_String)
      return Filesystem_String;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Integer)
      return Integer;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Float)
      return Float;
   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Boolean) return Boolean;
   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Default    : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance;
   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type;
   --  Same as above, except that if there are not enough parameters, Default
   --  is returned. Returned value must be freed.

   procedure Set_Error_Msg
     (Data : in out Callback_Data; Msg : String) is abstract;
   --  Set an error message.
   --  The return value for this callback will be ignored. On most languages
   --  (python,...) this is equivalent to raising an exception.
   --  If Msg is set to the empty string, an exception will still be raised

   procedure Set_Return_Value_As_List
     (Data  : in out Callback_Data;
      Size  : Natural := 0;
      Class : Class_Type := No_Class) is abstract;
   --  Setup the return value as an empty list. New values can be appended to
   --  the list with Set_Return_Value.
   --  It is possible to override the exact returned type by setting Class.
   --  This should however be a subclass of the builtin "list" for language
   --  in which it makes sense. This is often risky if one of the scripting
   --  languages your application cannot create subclasses of lists.
   --  If Size is not 0, then the list has a fixed size. Depending on the
   --  language, this could be a different type, such as a tuple in python.
   --
   --  See also the documentation for List_Instance for a full example
   --  returning a list to the scripting language.

   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Integer) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Float) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : String) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Boolean) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : Class_Instance) is abstract;
   procedure Set_Return_Value
     (Data : in out Callback_Data'Class; Value : Filesystem_String);
   --  Set the return value of Data.
   --  If the return value was set as a list, Value is appended to the
   --  list. For languages that do not support lists, the append is only
   --  performed for strings (newline-separated). Other data types simply
   --  replace the current return value.

   procedure Set_Return_Value_Key
     (Data : in out Callback_Data; Key : String; Append : Boolean := False)
      is abstract;
   procedure Set_Return_Value_Key
     (Data : in out Callback_Data; Key : Integer; Append : Boolean := False)
      is abstract;
   procedure Set_Return_Value_Key
     (Data   : in out Callback_Data;
      Key    : Class_Instance;
      Append : Boolean := False) is abstract;
   --  Move the current value of Data, as set by Set_Return_Value into a
   --  htable.
   --  Typical usage would be:
   --     Set_Return_Value (Data, 12);
   --     Set_Return_Value_Key (Data, "key1");
   --
   --     Set_Return_Value_As_List (Data);
   --     Set_Return_Value (Data, 1);
   --     Set_Return_Value (Data, 2);
   --     Set_Return_Value_Key (Data, "key2");
   --  will create a htable containing (key1 => 12, key2 => (1, 2))
   --
   --  If Append is true and there is already a value set for Key, then the new
   --  value is append to it (a list is created if necessary). This might not
   --  be supported for languages that do not explicitly support htables like
   --  the GPS shell.
   --
   --  No provision is made for creating htables of htables, although htables
   --  of lists are supported, or for getting the currently set value for Key.

   function Return_Value (Data : Callback_Data) return String is abstract;
   function Return_Value (Data : Callback_Data) return Integer is abstract;
   function Return_Value (Data : Callback_Data) return Float is abstract;
   function Return_Value (Data : Callback_Data) return Boolean is abstract;
   function Return_Value
     (Data : Callback_Data) return Class_Instance is abstract;
   --  Return the value returned by a script function, via a call to
   --  Execute_Command below.
   --  If the type you are requesting is not compatible with the actual
   --  returned value, Invalid_Parameter is raised.
   --  See also Return_Value below, which returns a List_Instance'Class.

   -----------
   -- Lists --
   -----------

   subtype List_Instance is Callback_Data'Class;
   --  Represents a list passed as parameter.
   --  In the context of a list, Set_Nth_Arg will always append to the list if
   --  the given index is outside of the current range of the list.
   --
   --  To return a list to the scripting language, you can therefore do the
   --  following:
   --
   --    procedure Handler (Data : in out Callback_Data'Class; Cmd : String) is
   --       List : List_Instance := New_List (Get_Script (Data));
   --    begin
   --       Set_Nth_Arg (List, Natural'Last, 12);
   --       Set_Nth_Arg (List, Natural'Last, "value");
   --       Set_Return_Value (Data, List);
   --    end;
   --
   --  The handling of the list can be made transparent by using the following
   --  construct:
   --
   --    procedure Handler (Data : in out Callback_Data'Class; Cmd : String) is
   --    begin
   --       Set_Return_Value_As_List (Data);
   --       Set_Return_Value (Data, 12);
   --       Set_Return_Value (Data, "value");
   --    end;
   --
   --  However, this second approach does not let you return lists of list,
   --  for instance, which is doable with the first approach.

   function New_List
     (Script : access Scripting_Language_Record;
      Class  : Class_Type := No_Class)
      return List_Instance'Class is abstract;
   --  Creates a new empty list
   --  It is possible to override the exact returned type by setting Class.
   --  This should however be a subclass of the builtin "list" for language
   --  in which it makes sense. This is often risky if one of the scripting
   --  languages your application cannot create subclasses of lists.

   function Nth_Arg
     (Data : Callback_Data; N : Positive)
      return List_Instance'Class is abstract;
   --  Get a list parameter. The default value is always the empty list, but
   --  you can still get an Invalid_Parameter exception if the corresponding
   --  parameter is not a list.
   --  In the case of python, this function will accept any iterable type (a
   --  list, a tuple, a user-defined type with a __iter__ method, even a
   --  dictionary or a string).

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return List_Instance;
   function Execute
     (Subprogram : access Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean)
      return List_Instance'Class is abstract;
   --  Execute a subprogram and assumes it returns a list.
   --  The resulting List must be freed by the caller.

   function Return_Value
     (Data : Callback_Data) return List_Instance'Class is abstract;
   --  Returns the list returned by a command (see Execute_Command).

   procedure Set_Nth_Arg
     (Data : in out Callback_Data;
      N : Positive; Value : List_Instance) is abstract;
   --  Override the nth arg in Data

   procedure Set_Return_Value
     (Data : in out Callback_Data; Value : List_Instance) is abstract;
   --  Set the value returned to the shell

   ---------------------
   -- Class instances --
   ---------------------

   Invalid_Data : exception;

   function New_Instance
     (Script : access Scripting_Language_Record; Class : Class_Type)
      return Class_Instance is abstract;
   --  Create a new instance of the class.
   --  No data is stored in the object.
   --  This call should generally be the result of the user calling a
   --  function, which acts as a constructor for the class.
   --  The instance constructor (Constructor_Method) is not called, even
   --  though the instance has been properly initialized. You should therefore
   --  perform any initialization manually just after calling New_Instance.

   function Get_Method
     (Instance : Class_Instance; Name : String) return Subprogram_Type;
   --  Return the method of instance Instance. Returned value must be freed by
   --  the caller.
   --  Parameters passed to the return value must not specify the instance as
   --  first parameter.

   function Is_Subclass
     (Instance : Class_Instance; Base : Class_Type) return Boolean;
   function Is_Subclass
     (Instance : Class_Instance; Base : String) return Boolean;
   --  Whether Instance is a Base or from a subclass of Base

   function Get_Script (Instance : Class_Instance) return Scripting_Language;
   --  Return the scripting language that created this instance

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Integer;
   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Float;
   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return String;
   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Boolean;
   --  Get the data embedded in the class.
   --  These are specialized cases of Get_Data below.
   --  Invalid_Data is raised if no such data was stored in the instance.
   --  Constraint_Error is raised if the data is not of the appropriate type.
   --  Class is used to differentiate the data for instances that inherit from
   --  several GPS classes, as in:
   --     class Foo (GPS.Console, GPS.Process):
   --        def __init__ (self):
   --           GPS.Console.__init__ (self,..)
   --           GPS.Process.__init__ (self,...)
   --  since both internal classes expect different data stored internally

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : String);
   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Integer);
   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Float);
   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Boolean);
   --  Associate some data with the instance.
   --  These are specialized cases of Set_Data below.
   --  The class name is required to handle multiple inheritance: if we were
   --  always using the same internal identifier to associated data with the
   --  instance, then we couldn't have a class with multiple ancestors, each
   --  expecting its own user data set in the constructor.

   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Integer);
   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Float);
   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : String);
   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Boolean);
   --  Export a field stored in the instance.
   --  The way to access it depends on the language:
   --    - in the GPS shell, you need to prefix its name with "@", as in:
   --        > Console "foo"    # Create new instance
   --        > @id %1           # Access its "id" property
   --    - in Python, this is used with the usual python conventions:
   --        > c = Console ("foo")
   --        > c.id
   --  The value of the field can be overridden in the scripting language, but
   --  this change will not be reflected in Ada. For instance, in python:
   --       c.id = 2
   --  is valid, but will have no effect on the Ada side.
   --
   --  If you want true read-only properties, you need to use Register_Property
   --  through getters and setters.
   --
   --  In Python, this procedure doesn't go through the class's __setattr_
   --  function.

   --------------------
   -- Instance lists --
   --------------------

   --  Most internal objects, when exported to a shell, should reuse the same
   --  class instance whenever the same physical object is referenced. This is
   --  so that the user can store user data within the instance, and get it
   --  back easily the next time the same object is referenced.
   --  For types derived from GObject_Record, we provide appropriate Set_Data
   --  and Get_Data subprograms. For other types, the instance_list type can
   --  be used to store the instances (of which there is one per scripting
   --  language).

   type Instance_List is private;
   Null_Instance_List : constant Instance_List;
   --  Stores the instance created for some GPS internal data, so that the same
   --  script instance is reused every time we reference the same Ada object.

   type Instance_List_Access is access all Instance_List;
   --  This type should be convertible to a System.Address for storage in
   --  a selection_context

   type Instance_Array is array (Natural range <>) of Class_Instance;
   type Instance_Array_Access is access Instance_Array;

   procedure Free (List : in out Instance_List);
   procedure Free (List : in out Instance_List_Access);
   --  Free the instances stored in the list

   function Get
     (List   : Instance_List;
      Script : access Scripting_Language_Record'Class) return Class_Instance;
   --  Return the instance for a given script

   procedure Set
     (List   : in out Instance_List;
      Script : access Scripting_Language_Record'Class;
      Inst   : Class_Instance);
   --  Set the instance for a specific language

   function Get_Instances (List : Instance_List) return Instance_Array;
   --  Return the instance array contained in the given list

   function Length (List : Instance_List_Access) return Natural;
   --  Return the number of instances that are stored in the list

   -------------------------
   -- Instance properties --
   -------------------------

   type Instance_Property_Record is abstract tagged null record;
   type Instance_Property is access all Instance_Property_Record'Class;

   procedure Destroy (Prop : in out Instance_Property_Record);
   --  Type of data that can be associated with a class_instance. This is a
   --  general type, but simpler types are provided already

   function Create_Property
     (Val : Boolean) return Instance_Property_Record'Class;
   function Create_Property
     (Val : Integer) return Instance_Property_Record'Class;
   function Create_Property
     (Val : Float) return Instance_Property_Record'Class;
   function Create_Property
     (Val : String) return Instance_Property_Record'Class;
   --  Return an instance of Instance_Property that wraps one of the basic
   --  types. The returned value must be Destroyed, unless you store it
   --  through Set_Data, in which case GNATCOLL will take care of that.

   function As_String (Prop : Instance_Property_Record'Class) return String;
   --  Assuming Prop was created with Create_Property, return its value

   procedure Set_Data
     (Instance : Class_Instance;
      Name     : String;
      Property : Instance_Property_Record'Class);
   --  Associate user data with Instance. Multiple data can be stored in a
   --  given instance, each associated with a different Name. Typically, GPS
   --  classes use the class name as the property name to avoid conflicts.
   --  When the property is no longer needed (either because it is replaced by
   --  another one with the same name, or because Instance is destroyed), the
   --  Destroy operation is called on Property.
   --  Note that a copy of Property is stored, not Property itself.
   --
   --  A simplified interface for some scalar types is also defined, see
   --  Set_Data above

   function Get_Data
     (Instance : Class_Instance;
      Name     : String) return Instance_Property;
   --  Return a general property associated with the widget.
   --  Return null if there is no such property.

   ---------------------------
   -- Class_Instance_Record --
   ---------------------------

   --  This type encapsulate some language specific data. It is overriden by
   --  each of the scripting languages. Do not use directly unless you are
   --  implementing a new scripting language

   type Class_Instance_Record is abstract tagged limited private;
   type Class_Instance_Record_Access is access all Class_Instance_Record'Class;
   --  A type overriden by each of the scripting languages

   function Is_Subclass
     (Instance : access Class_Instance_Record;
      Base     : String) return Boolean is abstract;
   --  Whether Instance is a Base or from a subclass of Base. Do not use
   --  directly, use the version that takes a Class_Instance instead

   function Get_CIR
     (Inst : Class_Instance) return Class_Instance_Record_Access;
   --  For internal use only

   function Get_Method
     (Inst : access Class_Instance_Record;
      Name : String) return Subprogram_Type is abstract;

   function Print_Refcount
     (Instance : access Class_Instance_Record) return String;
   --  Debug only: print the reference counting for this instance.
   --  Implementations are encourage to concatenate with the inherited
   --  method's result

   procedure Set_Property
     (Instance : access Class_Instance_Record;
      Name     : String; Value : Integer) is abstract;
   procedure Set_Property
     (Instance : access Class_Instance_Record;
      Name     : String; Value : Float) is abstract;
   procedure Set_Property
     (Instance : access Class_Instance_Record;
      Name     : String; Value : Boolean) is abstract;
   procedure Set_Property
     (Instance : access Class_Instance_Record;
      Name     : String; Value : String) is abstract;
   --  See definition of Set_Constant (Class_Instance)

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Name     : String;
      Property : Instance_Property_Record'Class);
   function Get_Data
     (Instance : access Class_Instance_Record'Class;
      Name     : String) return Instance_Property;
   --  Internal version of Set_Data/Get_Data.
   --  For internal use only

   -------------------------
   -- Callback_Data lists --
   -------------------------

   --  This type's goal is similar to the one for the instance lists, since the
   --  callback_data are also language-specific

   type Callback_Data_List is private;
   --  Stores a list of callback_data, each associated with a different
   --  scripting language

   procedure Free (List : in out Callback_Data_List);
   --  Free the instances stored in the list

   function Get
     (Repo   : access Scripts_Repository_Record'Class;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access;
   --  Return the data for a given script.
   --  The returned value should not be freed by the caller, it is the
   --  responsability of the callback_data_list to do so.

   procedure Set
     (Repo   : access Scripts_Repository_Record'Class;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access);
   --  Set the data for a specific language. Data should not be freed by the
   --  caller.

   ---------------
   --  Consoles --
   ---------------
   --  When executing script commands, they will very often produce some
   --  output, including possibly error or log messages. The following class
   --  acts as a small wrapper around more advanced types of console, like a
   --  text-mode console, or a GtkAda console. This type is used so that the
   --  subprograms below can be used both in graphical and textual mode

   type Virtual_Console_Record is abstract tagged private;
   type Virtual_Console is access all Virtual_Console_Record'Class;

   procedure Insert_Text
     (Console : access Virtual_Console_Record; Txt : String) is abstract;
   --  Prints some output in the console

   procedure Insert_Log
     (Console : access Virtual_Console_Record; Txt : String) is null;
   pragma Obsolescent (Insert_Log);
   --  ignored, kept for backward compatibility only

   procedure Insert_Error
     (Console : access Virtual_Console_Record; Txt : String) is abstract;
   --  Prints an error message resulting from the wrong execution of a script

   procedure Insert_Prompt
     (Console : access Virtual_Console_Record; Txt : String) is abstract;
   --  Display Txt as a new prompt in the console

   procedure Ref   (Console : access Virtual_Console_Record) is null;
   procedure Unref (Console : access Virtual_Console_Record) is null;
   --  Increment or decrement the reference counting for the console, if that
   --  notion makes sense for that particular console.
   --  The idea is that when we are temporary using a different console for the
   --  output, we do not want the default console to be destroyed
   --  automatically, in case its only reference was hold by the scripting
   --  language.

   procedure Grab_Events
     (Console : access Virtual_Console_Record;
      Grab    : Boolean) is null;
   --  Make sure all graphical events go to the console instead of the rest of
   --  the application.
   --  This is mostly used to avoid recursive re-entrant calls to the script
   --  interpreter.

   procedure Set_As_Default_Console
     (Console : access Virtual_Console_Record;
      Script  : Scripting_Language := null) is null;
   --  Called when Console becomes the default console for the scripting
   --  language Script.
   --  Script might be null when the Console is no longer the default console
   --  for that script.

   procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Virtual_Console_Record) is abstract;
   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Virtual_Console_Record)
      return Class_Instance is abstract;
   --  Associate a console and class instances, so that a given instance is
   --  always associated with the same class instance.
   --  Typical example of implementation would be:
   --      type My_Console is new Virtual_Console_Record with record
   --          Instances : Instance_List;
   --      end record;
   --
   --      procedure Set_Data_Primitive (...) is
   --      begin
   --         Set (Console.Instances, Get_Script (Instance), Instance);
   --      end Set_Data_Primitive;
   --
   --      function Get_Instance (...) is
   --      begin
   --          return Get (Console.Instances, Script);
   --      end Get_Instance;

   procedure Set_Data
     (Instance : Class_Instance;
      Console  : access Virtual_Console_Record'Class);
   function Get_Data (Instance : Class_Instance) return Virtual_Console;
   --  Return the virtual console stored in Instance

   procedure Process_Pending_Events_Primitive
     (Console : access Virtual_Console_Record) is null;
   procedure Process_Pending_Events
     (Console : access Virtual_Console_Record'Class);
   --  Process all pending graphical events, so that the application is
   --  properly refreshed while a script is running.
   --  This package will properly make sure this function is not called too
   --  often, so you don't need to do additional work for that

   procedure Clear
     (Console    : access Virtual_Console_Record) is null;
   --  Clear the contents of the console

   function Read
     (Console    : access Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean;
      Prompt     : String) return String;
   function Read
     (Console    : access Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String;
   --  Return at most Size characters from the console.
   --  If Whole_Line is true, the returned value stops at the first newline
   --  character seen in any case.
   --  If Prompt is specified, it is displayed first (via Insert_Prompt).

   -------------------------
   -- Scripting languages --
   -------------------------

   type Module_Command_Function is access procedure
     (Data : in out Callback_Data'Class; Command : String);
   --  The callback handler for a command.
   --  The first argument is always the instance to which the method applies,
   --  if Command is a method.
   --  Should raise Invalid_Parameters if one of the parameters is incorrect.
   --  The number of parameters has been checked before this procedure is
   --  called.

   procedure Destroy (Script : access Scripting_Language_Record) is null;
   --  Destroy the scripting language and the memory it occupies

   type Param_Descr is private;
   type Param_Array is array (Natural range <>) of Param_Descr;
   type Param_Array_Access is access all Param_Array;
   No_Params : constant Param_Array;
   --  Description of a parameter

   function Param
     (Name : String; Optional : Boolean := False) return Param_Descr;
   --  Describe one of the parameters of a script function

   type Command_Descr;
   type Command_Descr_Access is access all Command_Descr;
   type Command_Descr (Length : Natural) is record
      Command       : String (1 .. Length);
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Params        : Param_Array_Access;
      Static_Method : Boolean := False;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Next          : Command_Descr_Access;
   end record;
   --  Params is left to null if the user did not specify the name of
   --  parameters in the call to Register_Command (this is different from
   --  having a non-null but empty Params, which indicates there are no
   --  parameters).

   procedure Register_Command
     (Script  : access Scripting_Language_Record;
      Command : Command_Descr_Access) is abstract;
   --  Register a new callback for a command.
   --  Command will exist as long as Script, so it is safe (and recommended)
   --  that script points to Command instead of duplicating the data. This
   --  saves memory by sharing storage among all the scripting languages.
   --  See also Register_Command applied to the script_repository for more
   --  information.

   type Property_Descr;
   type Property_Descr_Access is access all Property_Descr;
   type Property_Descr (Length : Natural) is record
      Name   : String (1 .. Length);
      Class  : Class_Type;
      Setter : Module_Command_Function;
      Getter : Module_Command_Function;
      Next   : Property_Descr_Access;
   end record;
   --  The setter passes two parameters: first one is the instance, second one
   --  is the value of the property. Note that the property is untyped: you
   --  might have to try the various Nth_Arg to find out which type the user
   --  has passed. You can use Set_Error_Message if the property does not have
   --  the expected type.
   --
   --  The getter passes one parameter in Callback, which is the instance on
   --  which the property applies.
   --  It should call Set_Return_Value to return the value of the property.
   --
   --  You can potentially use the same callback in both cases, and count the
   --  number of arguments to find out whether the user is querying or setting
   --  the property.

   procedure Register_Property
     (Script : access Scripting_Language_Record;
      Prop   : Property_Descr_Access) is abstract;
   --  See documentation of Register_Property applied on the Scripts_Repository

   procedure Register_Class
     (Script : access Scripting_Language_Record;
      Name   : String;
      Base   : Class_Type := No_Class) is abstract;
   --  Create a new class in the interpreter

   procedure Block_Commands
     (Script : access Scripting_Language_Record;
      Block  : Boolean) is abstract;
   --  If Block is true, no command can be executed for this scripting language

   procedure Set_Default_Console
     (Script  : access Scripting_Language_Record;
      Console : Virtual_Console);
   --  Defines the console to use to display output, when none is specified
   --  to Execute_Command below

   function Get_Default_Console
     (Script : access Scripting_Language_Record) return Virtual_Console;
   --  Return the default console used for all outputs by this scripting
   --  language

   procedure Display_Prompt
     (Script  : access Scripting_Language_Record;
      Console : Virtual_Console := null) is null;
   --  Display the prompt on the script's default console. It uses
   --  Display_Prompt to compute the prompt to display.

   function Get_Prompt
     (Script : access Scripting_Language_Record) return String is abstract;
   --  Return the prompt to display

   procedure Execute_Command
     (Script       : access Scripting_Language_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean) is abstract;
   --  Execute a command in the script language.
   --  It isn't possible to retrieve the result of that command, this command
   --  is only used for its side effect.
   --  Depending on the language, Command might be a list of commands to
   --  execute, often semicolon or newline separated.
   --  Errors is set to True if there was any error executing the script.
   --
   --  The result of the command, as well as the text of the command itself,
   --  are not visible to the user if Hide_Output is True. Otherwise, the text
   --  is sent to Console. Any output done by the command, however (via "print"
   --  or "sys.stdout.write" statements for instance in python) will be
   --  displayed.
   --
   --  If Show_Command is True and Hide_Output is False, then the command
   --  itself is also printed in the console

   function Execute_Command
     (Script       : access Scripting_Language_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   --  Execute a command, and return its output as a displayable string.
   --  Note: some languages might simply return an empty string if they cannot
   --  capture the output of their interpreter. This command is mostly useful
   --  for the GPS shell, but also supported by python.
   --  Command can never be a list of commands (no semicolon or newline
   --  separated).

   function Execute_Command
     (Script      : access Scripting_Language_Record;
      CL          : Arg_List;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean is abstract;
   --  Execute a command and evaluate its return value (*not* its output) as a
   --  boolean. This is different from the version returning a string, in that
   --  only the return value is considered, not the full output.

   procedure Execute_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean);
   function Execute_Command
     (Script      : access Scripting_Language_Record;
      Command     : String;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean;
   function Execute_Command
     (Script       : Scripting_Language;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String;
   --  Same as above, working direcly on a String. This String is interpreted
   --  as a command line using the mechanism described in
   --  GNATCOLL.Command_Lines.
   --  These are only provided for backward compatibility and you should use
   --  directly the version that takes a Command_Line whenever possible.

   function Execute_Command
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : Callback_Data'Class) return Boolean is abstract;
   --  Execute a command, the argument of which are specified separately in
   --  Args.
   --  Return the value returned by the command itself.

   Error_In_Command : exception;

   procedure Execute_Command
     (Args        : in out Callback_Data;
      Command     : String;
      Hide_Output : Boolean := True) is abstract;
   --  Execute the given function passing one or more arguments via Args.
   --  On exit, Args is modified to contain the value returned by the command.
   --  If you know the expected result type, you can then use the Return_Value
   --  functions above to retrieve the values.
   --     declare
   --        C : Callback_Data'Class := Create (Script, 1);
   --     begin
   --        Set_Nth_Arg (C, 1, "some value");
   --        Execute_Command (C, "somefunction");
   --        Put_Line (Return_Value (C));  --  If returned a string
   --        Put_Line (Integer'Image (Return_Value (C)));  --  If an integer
   --
   --        declare
   --           L : List_Instance'Class := Return_Value (C);  --  If a list
   --        begin
   --           for Item in 1 .. Number_Of_Arguments (L) loop
   --              Put_Line (Nth_Arg (L, Item));  --  A list of strings ?
   --           end loop;
   --        end;
   --     end;
   --
   --  If the command returns an error (or raised an exception), an Ada
   --  exception is raised in turn (Error_In_Command). The exception is also
   --  printed on the current console for the language.

   function Execute_Command_With_Args
     (Script : access Scripting_Language_Record;
      CL     : Arg_List) return String;
   --  Execute a command.
   --  This procedure needs only be implemented for the GPS shell, in all other
   --  language you should keep the default which raises Program_Error, since
   --  this function is not used anywhere but for shell commands.
   --  All output is hidden

   procedure Execute_File
     (Script       : access Scripting_Language_Record;
      Filename     : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean) is abstract;
   --  Execute a script contained in an external file

   type Script_Loader is
     access function (File : GNATCOLL.VFS.Virtual_File) return Boolean;
   function Load_All (File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Given the name of a script, returns True if the script should be loaded

   procedure Load_Directory
     (Script    : access Scripting_Language_Record;
      Directory : GNATCOLL.VFS.Virtual_File;
      To_Load   : Script_Loader := Load_All'Access) is null;
   --  Load all scripts found in the given directory, and for which To_Load
   --  returns True.

   function Interrupt
     (Script : access Scripting_Language_Record) return Boolean;
   --  Interrupt the command currently executed.
   --  The interrupt need not be synchronous, but should occur as soon as
   --  possible.
   --  Returns True if the execution could be interrupt, False if there is no
   --  command being executed, or it can't be interrupted

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);
   package String_Lists_Sort is new String_Lists.Generic_Sorting;

   procedure Complete
     (Script      : access Scripting_Language_Record;
      Input       : String;
      Completions : out String_Lists.List);
   --  Provide the list of possible completion when the user has typed Input in
   --  a console. This completion can be as smart as possible, but can also
   --  return an empty list if that scripting language doesn't support
   --  completion.

   function Get_Name (Script : access Scripting_Language_Record)
      return String is abstract;
   --  The name of the scripting language

   function Get_Repository (Script : access Scripting_Language_Record)
      return Scripts_Repository is abstract;
   --  Return the kernel in which Script is registered

   function Current_Script
     (Script : access Scripting_Language_Record) return String is abstract;
   --  Return the name of the current script (file or inline script) that we
   --  are executing. When unknown, the empty string should be returned.

   --------------------------
   -- Commands and methods --
   --------------------------

   Constructor_Method  : constant String;
   Addition_Method     : constant String;
   Substraction_Method : constant String;
   Destructor_Method   : constant String;

   Comparison_Method   : constant String;
   --  Should return -1, 0 or 1 depending on whether A<B, A==B or A>B

   Equal_Method        : constant String;
   --  Should return a boolean, testing for equality.
   --  Note that in python, at least, definining this will not automatically
   --  define the inequality, so it might be better to use Comparison_Method
   --  instead.

   procedure Destroy (Repo : in out Scripts_Repository);
   --  Free all memory associated with the repository

   procedure Register_Standard_Classes
     (Repo               : access Scripts_Repository_Record'Class;
      Console_Class_Name : String;
      Logger_Class_Name  : String := "");
   --  Register predefined classes that are needed for support of consoles.
   --  If Logger_Class_Name, this also creates a new class to interface with
   --  the GNATCOLL.Traces mechanism. This is especially useful if your own
   --  application is also uses the same mechanism.

   function Get_Console_Class
     (Repo : access Scripts_Repository_Record'Class) return Class_Type;
   --  Return the class to use for Console input/output.
   --  This is only initialized when Register_Standard_Classes is called

   procedure Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Params        : Param_Array;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "");
   procedure Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Minimum_Args  : Natural    := 0;
      Maximum_Args  : Natural    := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "");
   --  Add a new function to all currently registered script languages.
   --
   --  The first version is recommended. By contrast, you will need to call
   --  Name_Parameters yourself in the Handler for the second version.
   --
   --  Params should not be freed by the caller.
   --
   --  If Class is not No_Class, then this procedure creates a method for this
   --  class, for the languages for which this is appropriate. An extra
   --  parameter is automatically added to the command, in first position,
   --  which is the instance to which this applies. In some shells, the user
   --  must provide this himself (GPS shell for instance), since the language
   --  is not object oriented. This first parameter must not be counted in
   --  Minimum_args and Maximum_Args
   --  Otherwise, it creates a global function in the script language.
   --
   --  If Static_Method is True, then Class must be different from No_Class.
   --  The resulting method doesn't take an instance as its first
   --  parameter. Instead, it behaves like a global function, except it is in a
   --  specific namespace corresponding to the class name.
   --  This is similar to C++'s static methods.
   --
   --  If Command is Constructor_Method, then the function is setup as the
   --  constructor for Class, which must not be No_Class. For compatibility
   --  with the greater number of languages, only one such constructor can be
   --  defined per class.
   --  A constructor receives an already built instance of the object, and
   --  should initialize the fields. Its first parameter is the instance, the
   --  second, third,... are the parameters passed to the constructor.
   --  The constructor shouldn't return any value through Set_Return_Value.
   --
   --  If Command is Addition_Method, this is a function that should take one
   --  argument in addition to the instance, and return a new instance. This
   --  handles statements like "inst + 1", although the second argument can be
   --  of any type (you can even handle multiple types in your implementation)
   --
   --  Subscription_Method is similar to Addition_Method.
   --
   --  Comparison_Method is a function that takes a second parameter, and
   --  returns -1 if the first is less than the second, 0 if they are equal,
   --  and 1 if the first is greater than the second.
   --
   --  Destructor_Method is called just before the instance is destroyed
   --
   --  Description of the new command must be put in the file
   --  shell_commands.xml, which is read dynamically when generating the
   --  documentation.
   --
   --  If the command has some graphical output (dialog,...), it must run in
   --  a separate main loop (Gtk.Main.Gtk_Main or modal dialogs).
   --
   --  Language can be specified to restrict the command to a specific
   --  scripting language.

   procedure Register_Property
     (Repo   : access Scripts_Repository_Record'Class;
      Name   : String;
      Class  : Class_Type;
      Setter : Module_Command_Function := null;
      Getter : Module_Command_Function := null);
   --  Defines a property which is accessed through methods.
   --  If Setter is null, the property is read-only.
   --  If Getter is null, the property is write-only.
   --
   --  A property is very similar to two functions, but the syntax might be
   --  different. For instance:
   --    - In python:
   --        c = Console()       # Create instance
   --        c.msg = "message"   # Calls the setter
   --        print c.msg         # Calls the getter
   --      A function would have been:
   --        c.set_msg("message")
   --        print c.get_msg()
   --
   --    - In shell:
   --        Console             # create instance
   --        @msg %1 "message"   # Calls the setter
   --        @msg %2             # Calls the getter
   --      A function would have been:
   --        Console.set_msg %1 "message"
   --        Console.get_msg %2

   procedure Block_Commands
     (Repo  : access Scripts_Repository_Record'Class;
      Block : Boolean);
   --  Block all execution of shell commands if Block is true

   procedure Register_Scripting_Language
     (Repo   : access Scripts_Repository_Record'Class;
      Script : access Scripting_Language_Record'Class);
   --  Register a new scripting language in the kernel.
   --  Scripting languages are freed when the kernel is destroyed

   function Lookup_Scripting_Language
     (Repo : access Scripts_Repository_Record'Class;
      Name : String) return Scripting_Language;
   --  Lookup one of the registered languages by name

   type Scripting_Language_Array is
     array (Natural range <>) of Scripting_Language;
   function Get_Scripting_Languages
     (Repo : access Scripts_Repository_Record'Class)
      return Scripting_Language_Array;
   --  Return the list of all registered languages

   No_Args : constant GNAT.OS_Lib.Argument_List := (1 .. 0 => null);

private

   Constructor_Method  : constant String := "<@constructor@>";
   Addition_Method     : constant String := "+";
   Substraction_Method : constant String := "-";
   Comparison_Method   : constant String := "<=>";
   Destructor_Method   : constant String := "<@destructor@>";
   Equal_Method        : constant String := "==";

   type Virtual_Console_Record is abstract tagged record
      Hide_Output     : Boolean := False;
      Refresh_Timeout : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   type Class_Type is record
      Name   : GNAT.Strings.String_Access;

      Exists : Boolean := True;
      --  Set to False when the class is found using Lookup_Class. This is for
      --  instance the case for builtin classes.
   end record;

   type User_Data;
   type User_Data_List is access User_Data;

   type User_Data (Length : Natural) is record
      Next : User_Data_List;
      Name : String (1 .. Length);
      Prop : Instance_Property;
   end record;

   procedure Free_User_Data_List (Data : in out User_Data_List);
   --  Free the whole contents of the list

   type Param_Descr is record
      Name     : GNAT.Strings.String_Access;
      Optional : Boolean := False;
   end record;

   No_Params : constant Param_Array := (1 .. 0 => <>);

   type Class_Instance_Record is abstract tagged limited record
      Script    : Scripting_Language;

      Refcount  : aliased Interfaces.Integer_32 := 0;
      --  This is the reference counting only for the Ada side. When
      --  interfacing with python, for instance, the latter never owns any
      --  reference to the Class_Instance.
   end record;
   procedure Incref (Self : not null access Class_Instance_Record) is null;
   procedure Decref (Self : not null access Class_Instance_Record) is null;
   --  These subprograms are called when changing the reference counting of
   --  the Class_Instance that owns this Class_Instance_Record.
   --  These procedures are called after updating the reference count.

   function Get_User_Data
     (Self : not null access Class_Instance_Record)
      return access User_Data_List;
   --  Return the list of user data stored for this instance. Depending on the
   --  scripting language, this list might be stored in various places (as a
   --  python attribute, directly in Ada for the shell,...) This list is shared
   --  amonst the scripting languages.

   type Class_Instance_Data is new Ada.Finalization.Controlled with record
      Data : Class_Instance_Record_Access;
   end record;
   overriding procedure Adjust   (CI : in out Class_Instance_Data);
   overriding procedure Finalize (CI : in out Class_Instance_Data);
   function "=" (CI1, CI2 : Class_Instance_Data) return Boolean;
   --  Takes care of the reference counting for a Class_Instance

   type Class_Instance (Initialized : Boolean := False) is record
      case Initialized is
         when True  => Data : Class_Instance_Data;
         when False => null;
      end case;
   end record;
   --  A Class_Instance cannot be a visibly tagged type if declared in this
   --  package, since otherwise we have operations dispatching on multiple
   --  types.
   --  We use a discriminated type so that we can declare No_Class_Instance.
   --  Otherwise, Adjust is called before its body is seen.

   No_Class_Instance_Data : constant Class_Instance_Data :=
                              (Ada.Finalization.Controlled with Data => null);
   No_Class_Instance      : constant Class_Instance :=
                              (Initialized => False);

   No_Class  : constant Class_Type := (Name => null, Exists => False);
   Any_Class : constant Class_Type := (Name   => new String'("@#!-"),
                                       Exists => False);

   type Subprogram_Record is abstract tagged null record;
   type Callback_Data is abstract tagged null record;

   type Scripting_Language_Record is abstract tagged record
      Console : Virtual_Console;
   end record;

   type Instance_List is record
      List : Instance_Array_Access;
   end record;

   Null_Instance_List : constant Instance_List := (List => null);

   type Callback_Data_Array is
     array (Natural range <>) of Callback_Data_Access;
   type Callback_Data_List  is access Callback_Data_Array;

   type Scripting_Language_List is access Scripting_Language_Array;

   package Classes_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Class_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Scripts_Repository_Record is tagged record
      Scripting_Languages  : Scripting_Language_List :=
                               new Scripting_Language_Array'(1 .. 0 => null);
      Commands             : Command_Descr_Access;
      Properties           : Property_Descr_Access;
      Classes              : Classes_Hash.Map;
      Console_Class        : Class_Type := No_Class;
      Logger_Class         : Class_Type := No_Class;
   end record;

end GNATCOLL.Scripts;
