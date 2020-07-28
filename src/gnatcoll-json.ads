------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2020, AdaCore                     --
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

--  GNATCOLL.JSON exposes an API to parse and serialize data using the JSON
--  (JavaScript Object Notation) format.
--
--  Parsing JSON is as easy as calling the Read function::
--
--     Data : JSON_Value := Read ("[1, ""foo"", {""foo"": null}]");
--
--  Encoding to JSON is not any more complex::
--
--     JSON_String : String := Write (Data);
--
--  JSON trees (JSON_Value) are available for both inspection and
--  modification::
--
--     Float_Number : JSON_Value := Create (Float'(1.0));
--     --  Mere float number
--
--     Object : JSON_Value := Get (Get (Data), 3);
--     --  JSON object from Data: {"foo": null}
--
--     Some_Array : JSON_Value :=
--        Create (Float_Number & Object & Create (False));
--     --  Synthetic JSON array: [1.0, {"foo": null}, False]
--
--     --  Modify Data in place
--     Data.Append (Some_Array);

with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNATCOLL.Strings;

private with Ada.Containers.Vectors;
private with GNATCOLL.Atomic;

package GNATCOLL.JSON is

   type JSON_Value_Type is
     (JSON_Null_Type,
      --  Null value: all such JSON values are equivalent

      JSON_Boolean_Type,
      --  Boolean value: either true or false

      JSON_Int_Type,
      --  Integer value, encoded as an Ada Long_Long_Integer

      JSON_Float_Type,
      --  Float value, encoded as an Ada Long_Float

      JSON_String_Type,
      --  UTF-8 encoded string

      JSON_Array_Type,
      --  Array of JSON values

      JSON_Object_Type
      --  Sequence of fields. Each field has a unique name and maps to a
      --  JSON value. Depending on the context, this sequence can be processed
      --  as a mapping, because each field name is unique, but iterating on
      --  fields is deterministic because it is a sequence underneath.
   );
   --  Each JSON value (JSON_Value below) has a specific kind...

   subtype JSON_Elementary_Value_Type is JSON_Value_Type range
     JSON_Null_Type .. JSON_String_Type;
   --  Some are atoms...

   subtype JSON_Container_Value_Type is JSON_Value_Type range
     JSON_Array_Type .. JSON_Object_Type;
   --  While others are containers for other values

   Invalid_JSON_Stream : exception;

   subtype UTF8_String is String;
   type UTF8_String_Access is access all UTF8_String;

   subtype UTF8_Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   subtype UTF8_XString is GNATCOLL.Strings.XString;

   type JSON_Value is tagged private;
   --  Store a JSON value, which can be either a simple type (integer, string,
   --  ...) or an object with multiple fields, or an array (see JSON_Value_Type
   --  above).
   --
   --  This type has by-reference semantics, so using the standard assignment
   --  operator as in::
   --
   --      A := B;
   --
   --  just creates an alias. This means that modifying B will also modify A
   --  (and modifying A will of course modify B).
   --
   --  If you want to create a separate copy, you must use the Clone function.

   type JSON_Array is private with
      Iterable => (First       => Array_First,
                   Next        => Array_Next,
                   Has_Element => Array_Has_Element,
                   Element     => Array_Element);
   --  JSON array type. If an object of type JSON_Array is not otherwise
   --  initialized, it is initialized to Empty_Array.
   --
   --  Note that we use the Iterable aspect instead of the standard Ada 2012
   --  iterator aspects because the latter brings impossible constraints: Ada
   --  2012 iterators require JSON_Array to be tagged, which would break the
   --  existing API: the Get function would be dispatching over more than one
   --  type (JSON_Array because of the Arr argument, and JSON_Value because of
   --  the return type).

   JSON_Null : constant JSON_Value;
   Empty_Array : constant JSON_Array;

   --------------------
   -- Array handling --
   --------------------

   function Is_Empty (Arr : JSON_Array) return Boolean;
   --  Return whether Arr is an empty array

   function Length (Arr : JSON_Array) return Natural;
   --  Return the number of elements in Arr

   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value;
   --  If Arr has at least Index elements, return the element at that index.
   --  Raise a Constraint_Error otherwise.

   procedure Append (Arr : in out JSON_Array; Val : JSON_Value);
   --  Append Val as a new element at the end of the Arr array

   procedure Prepend (Arr : in out JSON_Array; Val : JSON_Value);
   --  Insert Val as the first element of the Arr array

   procedure Clear (Arr : in out JSON_Array);
   --  Remove all elements in Arr

   procedure Sort
     (Arr  : in out JSON_Array;
      Less : access function (Left, Right : JSON_Value) return Boolean);
   --  Reorder the elements in Arr such that they are sorted smallest first
   --  according to the strict comparison that Less implements.

   procedure Set_Element
     (Arr : in out JSON_Array; Index : Positive; Item : JSON_Value);
   --  If Arr has at least Index elements, replace the element at that index
   --  with Item. Raise a Constraint_Error otherwise.

   --  Both functions below are less efficient than Append because they
   --  result in an extra copy of the array, but they are easier to use when
   --  manipulating small arrays.

   function "&" (Arr : JSON_Array; Value : JSON_Value) return JSON_Array;
   --  Return a new array that appends Value to Arr

   function "&" (Value1, Value2 : JSON_Value) return JSON_Array;
   --  Return a new array that contains Value1 and Value2

   function Is_Empty (Val : JSON_Value) return Boolean;
   --  Return True if Val is empty array, empty object or null value. Return
   --  False in all other cases.

   ---------------------
   -- Array iteration --
   ---------------------

   function Array_First (Arr : JSON_Array) return Positive;
   function Array_Next (Arr : JSON_Array; Index : Positive) return Positive;
   function Array_Has_Element
     (Arr : JSON_Array; Index : Positive) return Boolean;
   function Array_Element
     (Arr : JSON_Array; Index : Positive) return JSON_Value;

   ----------------------------------------------
   -- Serialization/deserialization primitives --
   ----------------------------------------------

   type Parsing_Error is record
      Line, Column : Positive;
      --  Line and column numbers at which a parsing error is detected

      Message : UTF8_Unbounded_String;
      --  Short description of the parsing error
   end record;

   function Format_Parsing_Error (Error : Parsing_Error) return String;
   --  Return a human-readable string to describe Error

   type Read_Result (Success : Boolean := True) is record
      case Success is
         when True =>
            Value : JSON_Value;
         when False =>
            Error : Parsing_Error;
      end case;
   end record;

   function Read
     (Strm     : Ada.Strings.Unbounded.Unbounded_String;
      Filename : String := "<data>") return JSON_Value;
   function Read
     (Strm     : String;
      Filename : String := "<data>") return JSON_Value;
   --  Parse the JSON document in Strm and return it. On parsing error, print
   --  an error message referencing Filename on the standard output and raise
   --  an Invalid_JSON_Stream exception.

   function Read
     (Strm : Ada.Strings.Unbounded.Unbounded_String) return Read_Result;
   function Read (Strm : String) return Read_Result;
   --  Parse the JSON document in Strm and return it. If there is a parsing
   --  error, return the corresponding error information.

   function Write (Item : JSON_Value; Compact : Boolean := True) return String;
   function Write (Item : JSON_Value; Compact : Boolean := True)
                   return Ada.Strings.Unbounded.Unbounded_String;
   --  Return a string that encodes Item in JSON. Unless Compact is True, this
   --  creates an indented multi-line representation.

   -----------------------------
   -- Creation of JSON values --
   -----------------------------

   function Create return JSON_Value
      with Post => Create'Result.Kind = JSON_Null_Type;
   --  Create a 'null' JSON value

   function Create (Val : Boolean) return JSON_Value
      with Post => Create'Result.Kind = JSON_Boolean_Type;
   --  Create a boolean-typed JSON value

   function Create (Val : Integer) return JSON_Value
      with Post => Create'Result.Kind = JSON_Int_Type;
   function Create (Val : Long_Integer) return JSON_Value
      with Post => Create'Result.Kind = JSON_Int_Type;
   function Create (Val : Long_Long_Integer) return JSON_Value
      with Post => Create'Result.Kind = JSON_Int_Type;
   --  Create an integer-typed JSON value

   function Create (Val : Float) return JSON_Value
      with Post => Create'Result.Kind = JSON_Float_Type;
   --  Create a float-typed JSON value

   function Create (Val : Long_Float) return JSON_Value
      with Post => Create'Result.Kind = JSON_Float_Type;

   function Create (Val : UTF8_String) return JSON_Value
      with Post => Create'Result.Kind = JSON_String_Type;
   --  Create a string-typed JSON value

   function Create (Val : UTF8_Unbounded_String) return JSON_Value
      with Post => Create'Result.Kind = JSON_String_Type;
   --  Create a string-typed JSON value

   function Create (Val : UTF8_XString) return JSON_Value
      with Post => Create'Result.Kind = JSON_String_Type;
   --  Create a string-typed JSON value

   function Create (Val : JSON_Array) return JSON_Value
      with Post => Create'Result.Kind = JSON_Array_Type;
   --  Create a JSON value from the JSON array

   function Create_Object return JSON_Value
      with Post => Create_Object'Result.Kind = JSON_Object_Type;
   --  Create an empty object. Values need to be added using the below
   --  Set_Field methods.

   procedure Sort
     (Val  : in out JSON_Value;
      Less : access function (Left, Right : JSON_Value) return Boolean);
   --  If Val is a JSON array or a JSON object, reorder its elements/fields
   --  such that they are sorted smallest first according to the strict
   --  comparison that Less implements. Note that for JSON objects, field
   --  values are compared, not field names.

   procedure Append (Arr : JSON_Value; Item : JSON_Value)
      with Pre => Arr.Kind = JSON_Array_Type;
   --  Assuming Arr is a JSON array, append Item to it

   function Clone (Val : JSON_Value) return JSON_Value;
   --  Return a deep clone of Val.  Any later change in Val or its fields
   --  (recursively) will have no impact on the resulting value.

   function "=" (Left, Right : JSON_Value) return Boolean;
   --  Return whether Left and Right are structurally identical.
   --
   --  The actual contents is compared, not the pointers. So two objects
   --  constructed independently, with the same contents, will match. For JSON
   --  objects, the order for fields is irrelevant, for objects. It is relevant
   --  for arrays however.

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, add a new field or modify the existing
   --  one for the given Field_Name. The field value is Field afterwards.

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_XString;
      Field      : JSON_Value)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, add a new field or modify the existing
   --  one for the given Field_Name. The field value is Field afterwards.

   --  All the Set_Field overloads below are convenience shortcut that first
   --  create a JSON value from their Field argument and then call the above
   --  Set_Field procedures with the result.

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Boolean)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Integer)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Integer)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Float)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field_Long_Float
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Float)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String)
      with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array)
      with Pre => Val.Kind = JSON_Object_Type;
   --  This performs a a shallow copy of Field, so any change you do to the
   --  passed array for Field afterwards will not impact Val.

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Set Field only if it is not empty string

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Set Field only if it is not empty string

   procedure Set_Field_If_Not_Empty
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Set Field only if it is not empty array.
   --  This performs a a shallow copy of Field, so any change you do to the
   --  passed array for Field afterwards will not impact Val.

   procedure Unset_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, remove its field whose name matches
   --  Field_Name. Do nothing if there is no such a field.

   ------------------------------------------------------
   -- Conversions from JSON values to native Ada types --
   ------------------------------------------------------

   function Kind (Val : JSON_Value) return JSON_Value_Type;
   --  Return the kind corresponding to the Val JSON value

   function Get (Val : JSON_Value) return Boolean
      with Pre => Val.Kind = JSON_Boolean_Type;

   function Get (Val : JSON_Value) return Integer
      with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Long_Integer
      with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Long_Long_Integer
      with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Float
      with Pre => Val.Kind = JSON_Float_Type;

   function Get_Long_Float (Val : JSON_Value) return Long_Float
      with Pre => Val.Kind = JSON_Float_Type;

   function Get (Val : JSON_Value) return UTF8_String
      with Pre => Val.Kind = JSON_String_Type;

   function Get (Val : JSON_Value) return UTF8_Unbounded_String
      with Pre => Val.Kind = JSON_String_Type;

   function Get (Val : JSON_Value) return UTF8_XString
      with Pre => Val.Kind = JSON_String_Type;

   function Get (Val : JSON_Value) return JSON_Array
      with Pre => Val.Kind = JSON_Array_Type;

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, return whether it contains a field whose
   --  name is Field.

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Value
      with Pre => Val.Kind = JSON_Object_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return Boolean
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Boolean_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return Integer
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Int_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return Long_Integer
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Int_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return Float
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Float_Type;

   function Get_Long_Float
      (Val : JSON_Value; Field : UTF8_String) return Long_Float
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Float_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return UTF8_String
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_String_Type;

   function Get
     (Val : JSON_Value; Field : UTF8_String) return UTF8_Unbounded_String
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_String_Type;

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Array
      with Pre => Val.Kind = JSON_Object_Type
                  and then Get (Val, Field).Kind = JSON_Array_Type;

   ---------------
   -- Iteration --
   ---------------

   procedure Map_JSON_Object
     (Val : JSON_Value;
      CB  : access procedure (Name : UTF8_String; Value : JSON_Value))
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, call CB on all its fields

   generic
      type Mapped (<>) is private;
   procedure Gen_Map_JSON_Object
     (Val         : JSON_Value;
      CB          : access procedure
        (User_Object : in out Mapped;
         Name        : UTF8_String;
         Value       : JSON_Value);
      User_Object : in out Mapped)
      with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, call CB on all its field, passing the
   --  given User_Object from call to call.

private

   type JSON_Array_Internal;
   type JSON_Array_Access is access all JSON_Array_Internal;
   type JSON_Object_Internal;
   type JSON_Object_Access is access all JSON_Object_Internal;

   type Data_Type (Kind : JSON_Value_Type := JSON_Null_Type) is record
      case Kind is
         when JSON_Null_Type    => null;
         when JSON_Boolean_Type => Bool_Value : Boolean;
         when JSON_Int_Type     => Int_Value  : Long_Long_Integer;
         when JSON_Float_Type   => Flt_Value  : Long_Float;
         when JSON_String_Type  => Str_Value  : UTF8_XString;
         when JSON_Array_Type   => Arr_Value  : JSON_Array_Access;
         when JSON_Object_Type  => Obj_Value  : JSON_Object_Access;
      end case;
   end record;

   type JSON_Value is new Ada.Finalization.Controlled with record
      Data : Data_Type;
   end record;
   --  We cannot merge Data_Type and JSON_Value, because JSON_Value cannot
   --  have a discriminant with a default value.

   overriding procedure Adjust (Obj : in out JSON_Value);
   overriding procedure Finalize (Obj : in out JSON_Value);

   --  JSON Array definition:

   package Vect_Pkg is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => JSON_Value);

   type JSON_Array is record
      Vals : Vect_Pkg.Vector;
   end record;

   type JSON_Array_Internal is record
      Cnt  : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
      Arr  : JSON_Array;
   end record;

   Empty_Array : constant JSON_Array := (Vals => Vect_Pkg.Empty_Vector);

   --  JSON Object definition:

   type Object_Item is record
      Key : UTF8_XString;
      Val : JSON_Value;
   end record;

   package Object_Items_Pkg is new Ada.Containers.Vectors
     (Positive, Object_Item);

   type JSON_Object_Internal is record
      Cnt  : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
      Vals : Object_Items_Pkg.Vector;
   end record;

   JSON_Null : constant JSON_Value :=
      (Ada.Finalization.Controlled with others => <>);
   --  Can't call Create, because we would need to see the body of
   --  Initialize and Adjust.

end GNATCOLL.JSON;
