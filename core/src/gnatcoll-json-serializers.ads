--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--

--  Provides a sax-like interface to stream a JSON-formatted document

with Ada.Containers.Vectors;

package GNATCOLL.JSON.Serializers is

   JSON_Serialization_Error : exception;
   --  Invalid sequence of serialization operations or non-finite number.

   type Serializer is abstract tagged limited private;
   --  Emits compact JSON through the Write primitive, which receives fragments
   --  as serializer operations are called.
   --
   --  A new serializer is ready to emit one root value. Use Encode_*
   --  for strings and scalars, or Start_Object/Start_Array for a container.
   --  Close each container with the matching End_Object/End_Array.
   --
   --  For example, to write {"sources":["main.adb","foo.ads"]}:
   --
   --     W.Start_Object;
   --     W.Encode_Name ("sources");
   --     W.Start_Array;
   --     W.Encode_String ("main.adb");
   --     W.Encode_String ("foo.ads");
   --     W.End_Array;
   --     W.End_Object;
   --     W.Close;

   procedure Write (Self : in out Serializer; Content : String) is abstract;
   --  Called when new content is to be added to the JSON output

   procedure Start_Object (Self : in out Serializer);
   --  Start an object. Each member within this object requires a name followed
   --  by a value.

   procedure End_Object (Self : in out Serializer);
   --  Close the innermost container, which must be an object

   procedure Start_Array (Self : in out Serializer);
   --  Start an array

   procedure End_Array (Self : in out Serializer);
   --  Close the innermost container, which must be an array.

   procedure Encode_Name (Self : in out Serializer; Name : UTF8_String);
   --  Start a member of the currently open object, including an empty name.
   --  Names are emitted in call order and are not checked for uniqueness.

   procedure Encode_String (Self : in out Serializer; Value : UTF8_String);
   procedure Encode_String
     (Self : in out Serializer; Value : UTF8_Unbounded_String);
   procedure Encode_String (Self : in out Serializer; Value : UTF8_XString);
   procedure Encode_Boolean (Self : in out Serializer; Value : Boolean);
   procedure Encode_Number
     (Self : in out Serializer; Value : Long_Long_Integer);
   procedure Encode_Number (Self : in out Serializer; Value : Long_Integer);
   procedure Encode_Number (Self : in out Serializer; Value : Integer);
   procedure Encode_Number (Self : in out Serializer; Value : Long_Float);
   procedure Encode_Null (Self : in out Serializer);
   --  Encode the values in the json stream

   procedure Close (Self : in out Serializer);
   --  Call after emitting the root value and closing all its containers.
   --  Raises JSON_Serialization_Error if no root was written or a container
   --  remains open.
   --  Does not close containers or flush the output sink. Further output
   --  requires Reset.

   procedure Reset (Self : in out Serializer);
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

   type Serializer is abstract tagged limited record
      Stack        : Frame_Vectors.Vector;
      Root_Written : Boolean := False;
      Finished     : Boolean := False;
   end record;

end GNATCOLL.JSON.Serializers;
