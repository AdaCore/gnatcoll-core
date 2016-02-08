------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

private with Ada.Containers.Vectors;

package GNATCOLL.JSON is

   type JSON_Value_Type is
     (JSON_Null_Type,
      JSON_Boolean_Type,
      JSON_Int_Type,
      JSON_Float_Type,
      JSON_String_Type,
      JSON_Array_Type,
      JSON_Object_Type);

   Invalid_JSON_Stream : exception;

   subtype UTF8_String is String;
   type UTF8_String_Access is access all UTF8_String;

   subtype UTF8_Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   subtype JSON_Elementary_Value_Type is JSON_Value_Type range
     JSON_Null_Type .. JSON_String_Type;
   subtype JSON_Container_Value_Type is JSON_Value_Type range
     JSON_Array_Type .. JSON_Object_Type;

   type JSON_Value is tagged private;
   --  Stores a JSON value, which can be either a simple type (integer,
   --  string, ...) or an object with multiple fields, or an array.
   --
   --  This type works by reference. Using the standard assignment operator
   --  as in
   --      A := B;
   --  means that modifying B will also modify A (and modifying A will of
   --  course modify B).
   --
   --  If you want to create a separate copy, you must use the Clone function.

   type JSON_Array is private;

   JSON_Null : constant JSON_Value;
   Empty_Array : constant JSON_Array;

   --  Array handling
   function Is_Empty (Arr : JSON_Array) return Boolean;
   function Length (Arr : JSON_Array) return Natural;
   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value;
   procedure Append (Arr : in out JSON_Array; Val : JSON_Value);
   procedure Prepend (Arr : in out JSON_Array; Val : JSON_Value);
   procedure Clear (Arr : in out JSON_Array);
   procedure Sort
     (Arr : in out JSON_Array;
      Less : access function (Left, Right : JSON_Value) return Boolean);
   --  Reorders the elements of array such that the elements are sorted
   --  smallest first as determined by the strict comparison provided by
   --  function Less.

   procedure Set_Element
     (Arr : in out JSON_Array; Index : Positive; Item : JSON_Value);
   --  If Index is not in the array index range, then Constraint_Error is
   --  propagated. Otherwise Set_Element assigns the value New_Item to the
   --  element at position Index.

   function "&" (Arr : JSON_Array; Value : JSON_Value) return JSON_Array;
   function "&" (Value1, Value2 : JSON_Value) return JSON_Array;
   --  Create a new array. This is lesss efficient than Append because it
   --  results in an extra copy of the array, but is easier to use when
   --  manipulating small arrays.

   function Is_Empty (Val : JSON_Value) return Boolean;
   --  Returns True if Val is empty array, empty object or null value

   --  Read or write JSON values into strings

   function Read
     (Strm     : Ada.Strings.Unbounded.Unbounded_String;
      Filename : String := "<data>") return JSON_Value;
   function Read
     (Strm     : String;
      Filename : String := "<data>") return JSON_Value;
   function Write (Item : JSON_Value; Compact : Boolean := True) return String;
   function Write (Item : JSON_Value; Compact : Boolean := True)
                   return Ada.Strings.Unbounded.Unbounded_String;

   --  Creation of JSON values

   function Create return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Null_Type);
   --  Creates a 'null' JSON value

   function Create (Val : Boolean) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Boolean_Type);
   --  Creates a boolean-typed JSON value

   function Create (Val : Integer) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Int_Type);
   function Create (Val : Long_Integer) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Int_Type);
   function Create (Val : Long_Long_Integer) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Int_Type);
   --  Creates an integer-typed JSON value

   function Create (Val : Float) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Float_Type);
   --  Creates a float-typed JSON value

   function Create (Val : Long_Float) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Float_Type);

   function Create (Val : UTF8_String) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_String_Type);
   --  Creates a string-typed JSON value

   function Create (Val : UTF8_Unbounded_String) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_String_Type);
   --  Creates a string-typed JSON value

   function Create (Val : JSON_Array) return JSON_Value;
   pragma Postcondition (Kind (Create'Result) = JSON_Array_Type);
   --  Creates a JSON value from the JSON array

   function Create_Object return JSON_Value;
   pragma Postcondition (Kind (Create_Object'Result) = JSON_Object_Type);
   --  Creates an empty object. Values need to be added using the below
   --  Set_Field methods

   procedure Sort
     (Val : in out JSON_Value;
      Less : access function (Left, Right : JSON_Value) return Boolean);
   --  Reorders the elements of array or fields of object such that the
   --  values are sorted smallest first as determined by the strict comparision
   --  provided by function Less.

   procedure Append (Arr : JSON_Value; Item : JSON_Value);
   pragma Precondition (Arr.Kind = JSON_Array_Type);
   --  Append Arr only in case of it is an array, raise Constraint_Error
   --  otherwise.

   function Clone (Val : JSON_Value) return JSON_Value;
   --  Returns a deep clone of Val.
   --  Any change in Val or its fields (recursively) will have no impact
   --  on the resulting value.

   function "=" (Left, Right : JSON_Value) return Boolean;
   --  Compare to values.
   --  The actual contents is compared, not the pointers. So two objects
   --  constructed independently, with the same contents, will match.
   --  The order that fields were created is irrelevant, for objects.
   --  The order in arrays is relevant.

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Value);
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Adds or modifies the named field for the specified json object, using
   --  the Field value.

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Boolean);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Integer);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Integer);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Float);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field_Long_Float
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : Long_Float);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_String);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : UTF8_Unbounded_String);
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   procedure Set_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String;
      Field      : JSON_Array);
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Any change you do to the array afterward will not impact Val

   procedure Unset_Field
     (Val        : JSON_Value;
      Field_Name : UTF8_String);
   --  Unset the field with the given name, just as if Set_Field had never
   --  been called.

   --  Utility functions used to translate a JSON value into an ordinary object

   function Kind (Val : JSON_Value) return JSON_Value_Type;

   function Get (Val : JSON_Value) return Boolean;
   pragma Precondition (Kind (Val) = JSON_Boolean_Type);

   function Get (Val : JSON_Value) return Integer;
   pragma Precondition (Kind (Val) = JSON_Int_Type);

   function Get (Val : JSON_Value) return Long_Integer;
   pragma Precondition (Kind (Val) = JSON_Int_Type);

   function Get (Val : JSON_Value) return Long_Long_Integer;
   pragma Precondition (Kind (Val) = JSON_Int_Type);

   function Get (Val : JSON_Value) return Float;
   pragma Precondition (Kind (Val) = JSON_Float_Type);

   function Get_Long_Float (Val : JSON_Value) return Long_Float;
   pragma Precondition (Kind (Val) = JSON_Float_Type);

   function Get (Val : JSON_Value) return UTF8_String;
   pragma Precondition (Kind (Val) = JSON_String_Type);

   function Get (Val : JSON_Value) return UTF8_Unbounded_String;
   pragma Precondition (Kind (Val) = JSON_String_Type);

   function Get (Val : JSON_Value) return JSON_Array;
   pragma Precondition (Kind (Val) = JSON_Array_Type);

   function Has_Field (Val : JSON_Value; Field : UTF8_String) return Boolean;
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Tell whether the object val contains a field named Field

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Value;
   pragma Precondition (Kind (Val) = JSON_Object_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Boolean;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Boolean_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Integer;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Int_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Long_Integer;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Int_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return Float;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Float_Type);

   function Get_Long_Float
      (Val : JSON_Value; Field : UTF8_String) return Long_Float;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Float_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return UTF8_String;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_String_Type);

   function Get
     (Val : JSON_Value; Field : UTF8_String) return UTF8_Unbounded_String;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_String_Type);

   function Get (Val : JSON_Value; Field : UTF8_String) return JSON_Array;
   pragma Precondition
     (Kind (Val) = JSON_Object_Type
      and then Kind (Get (Val, Field)) = JSON_Array_Type);

   ---------------
   -- Iteration --
   ---------------

   procedure Map_JSON_Object
     (Val : JSON_Value;
      CB  : access procedure (Name : UTF8_String; Value : JSON_Value));
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Iterate over all fields of the object

   generic
      type Mapped is private;
   procedure Gen_Map_JSON_Object
     (Val         : JSON_Value;
      CB          : access procedure
        (User_Object : in out Mapped;
         Name        : UTF8_String;
         Value       : JSON_Value);
      User_Object : in out Mapped);
   pragma Precondition (Kind (Val) = JSON_Object_Type);
   --  Iter on all fields of the object, like Map_JSON_Object does,
   --  but the callback can return a value which is also returned by
   --  Gen_Map_JSON_Object itself.

private

   type JSON_Array_Access is access all JSON_Array;
   type JSON_Object_Internal;
   type JSON_Object_Access is access all JSON_Object_Internal;

   type Counter is access Natural;

   type Data_Type (Kind : JSON_Value_Type := JSON_Null_Type) is record
      case Kind is
         when JSON_Null_Type    => null;
         when JSON_Boolean_Type => Bool_Value : Boolean;
         when JSON_Int_Type     => Int_Value  : Long_Long_Integer;
         when JSON_Float_Type   => Flt_Value  : Long_Float;
         when JSON_String_Type  => Str_Value  : UTF8_Unbounded_String;
         when JSON_Array_Type   => Arr_Value  : JSON_Array_Access;
         when JSON_Object_Type  => Obj_Value  : JSON_Object_Access;
      end case;
   end record;

   type JSON_Value is new Ada.Finalization.Controlled with record
      Cnt  : Counter := null;
      Data : Data_Type;
   end record;

   overriding procedure Initialize (Obj : in out JSON_Value);
   overriding procedure Adjust (Obj : in out JSON_Value);
   overriding procedure Finalize (Obj : in out JSON_Value);

   --  JSON Array definition:

   package Vect_Pkg is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => JSON_Value);

   type JSON_Array is record
      Vals : Vect_Pkg.Vector;
   end record;

   Empty_Array : constant JSON_Array := (Vals => Vect_Pkg.Empty_Vector);

   --  JSON Object definition:

   type Object_Item is record
      Key : UTF8_Unbounded_String;
      Val : JSON_Value;
   end record;

   package Object_Items_Pkg is new Ada.Containers.Vectors
     (Positive, Object_Item);

   type JSON_Object_Internal is record
      Vals : Object_Items_Pkg.Vector;
   end record;

   JSON_Null : constant JSON_Value :=
      (Ada.Finalization.Controlled with others => <>);
   --  Can't call Create, because we would need to see the body of
   --  Initialize and Adjust.

   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Array, JSON_Array_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (JSON_Object_Internal, JSON_Object_Access);
   procedure Free is
     new Ada.Unchecked_Deallocation (Natural, Counter);

end GNATCOLL.JSON;
