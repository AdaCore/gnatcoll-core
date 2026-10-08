------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                      --
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

with Ada.Containers.Vectors;

generic
   with procedure Append (Text : UTF8_String);
package GNATCOLL.JSON.Writers is

   Writer_Error : exception;
   --  Invalid sequence of writer operations or non-finite number.

   type Writer is tagged limited private;
   --  Emits compact JSON through Append, which receives fragments as writer
   --  operations are called. The caller owns and buffers the output. Discard
   --  or reset the writer if Append raises an exception.
   --
   --  A new writer is ready to emit one root value. Use Value or Null_Value
   --  for a scalar, or Start_Object/Start_Array for a container. Close each
   --  container with the matching End_Object/End_Array, then call Finish.
   --  Call Reset before writing another document with the same writer.
   --
   --  For example, to write {"sources":["main.adb"]}:
   --
   --     W.Start_Object;
   --     W.Name ("sources");
   --     W.Start_Array;
   --     W.Value ("main.adb");
   --     W.End_Array;
   --     W.End_Object;
   --     W.Finish;

   procedure Start_Object (Self : in out Writer);
   --  Start an object as the root, an array element, or an object member's
   --  value. In the latter case, call Name first. Each member within this
   --  object requires Name followed by a value.

   procedure End_Object (Self : in out Writer);
   --  Close the innermost container, which must be an object. Any preceding
   --  Name must already have its value.

   procedure Start_Array (Self : in out Writer);
   --  Start an array as the root, an array element, or an object member's
   --  value. In the latter case, call Name first. Emit each array element
   --  directly with Value, Null_Value, Start_Object or Start_Array.

   procedure End_Array (Self : in out Writer);
   --  Close the innermost container, which must be an array.

   procedure Name (Self : in out Writer; Text : UTF8_String);
   --  Start a member of the currently open object, including an empty name.
   --  Follow with exactly one Value, Null_Value, Start_Object or Start_Array
   --  before the next Name or End_Object. Do not call Name at the root or
   --  inside an array. Names are emitted in call order and are not checked
   --  for uniqueness.

   procedure Value (Self : in out Writer; Item : UTF8_String);
   --  Write a JSON string with quoting and escaping. Like the other Value
   --  overloads and Null_Value, this writes the root, an array element, or
   --  the value of the preceding Name in an object.

   procedure Value (Self : in out Writer; Item : UTF8_Unbounded_String);
   procedure Value (Self : in out Writer; Item : UTF8_XString);
   procedure Value (Self : in out Writer; Item : Boolean);
   procedure Value (Self : in out Writer; Item : Long_Long_Integer);
   procedure Value (Self : in out Writer; Item : Long_Float);
   procedure Null_Value (Self : in out Writer);

   procedure Finish (Self : in out Writer);
   --  Call after emitting the root value and closing all its containers.
   --  Raises Writer_Error if no root was written or a container remains
   --  open. Does not close containers or flush the output sink. Further
   --  output requires Reset.

   procedure Reset (Self : in out Writer);
   --  Prepare to write a new document, discarding any unfinished state.
   --  Does not clear the output sink; the caller manages its contents.

private

   type Container_Kind is (Array_Kind, Object_Kind);
   type Frame is record
      Kind         : Container_Kind;
      First        : Boolean := True;
      Name_Pending : Boolean := False;
   end record;

   package Frame_Vectors is new Ada.Containers.Vectors (Positive, Frame);

   type Writer is tagged limited record
      Stack        : Frame_Vectors.Vector;
      Root_Written : Boolean := False;
      Finished     : Boolean := False;
   end record;

end GNATCOLL.JSON.Writers;
