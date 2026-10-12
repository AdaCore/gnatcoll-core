****************************
**JSON**: handling JSON data
****************************

.. index:: json
.. highlight:: ada

`JSON <https://en.wikipedia.org/wiki/JSON>`_ is a format often used on the web
to communicate between a server and a browser, or between servers. It plays a
similar role to XML, but it has a much lighter syntax. On the other hand, it
doesn't provide advanced features like validation, which XML provides.

The ``GNATCOLL.JSON`` package provides an Ada API to decode JSON data from
strings and to encode that data back to strings. It also allows one to create
and modify JSON data.

API overview
============

The entry point for this API is the ``JSON_Value`` data type.  JSON values can
be any of:

* a null value (``JSON_Null_Type``): all such JSON values are equivalent;
* a boolean value (``JSON_Boolean_Type``): either true or false;
* an integer value (``JSON_Int_Type``), they are encoded as an Ada
  ``Long_Long_Integer``;
* a floating point value (``JSON_Float_Type``), they are encoded as an Ada
  ``Long_Float``;
* an UTF-8 encoded string (``JSON_String_Type``);
* an array of JSON values (``JSON_Array_Type``);
* a JSON object (``JSON_Object_Type``), which is a sequence of fields.  Each
  field has a unique name and maps to a JSON value. Depending on the context,
  this sequence can be processed as a mapping, because each field name is
  unique, but iterating on fields is deterministic because it is a sequence
  underneath.

Parsing JSON is as easy as calling the ``Read`` function::

   Data : JSON_Value := Read ("[1, ""foo"", {""foo"": null}]");

Encoding to JSON is not any more complex::

   JSON_String : String := Write (Data);

JSON trees (``JSON_Value``) are available for both inspection and
modification::

   Float_Number : JSON_Value := Create (Float'(1.0));
   --  Mere float number

   Object : JSON_Value := Get (Get (Data), 3);
   --  JSON object from Data: {"foo": null}

   Some_Array : JSON_Value :=
      Create (Float_Number & Object & Create (False));
   --  Synthetic JSON array: [1.0, {"foo": null}, False]

   --  Modify Data in place
   Data.Append (Some_Array);

Examples
========

Here is a complete program demonstrating the use of this API::

   with Ada.Text_IO;   use Ada.Text_IO;
   with GNATCOLL.JSON; use GNATCOLL.JSON;

   procedure JSON_Test is
      --  Create a JSON value from scratch
      My_Obj : JSON_Value := Create_Object;
   begin
      My_Obj.Set_Field ("field1", Create (Integer'(1)));
      My_Obj.Set_Field ("name", "theName");

      --  Now serialize it. The call below will display:
      --    {"field1": 1, "name": "thename"}
      Put_Line (My_Obj.Write);
   end JSON_Test;

The above uses the Ada 2005 "dot notation" to call primitive operations
(``.Set_Field``, ``.Write``), but naturally the more traditional "prefix
notation" is also available::

   Set_Field (My_Obj, "field1", Create (Integer'(1)));

It is also possible to create JSON arrays. These are not tagged types, so the
prefix notation has to be used. Here is a further example that sets another
field in the object we had before (``My_Obj``)::

   declare
      --  Create a JSON array
      My_Arr : JSON_Array := Empty_Array;
   begin
      --  Fill it
      Append (My_Arr, Create (Integer'(1)));
      Append (My_Arr, Create ("aString"));

      --  Create a field in My_Obj to hold this array
      My_Obj.Set_Field ("vals", My_Arr);

      --  This will now display:
      --    {"field1": 1, "name": "thename", "vals": [1, "aString"]}
      Put_Line (My_Obj.Write);
   end;

Similarly to containers from the standard Ada library (from
``Ada.Containers``), ``GNATCOLL.JSON`` features automatic memory management.
This means that there is no need for explicit destructors.

The above is all that is needed for most uses of ``GNATCOLL.JSON``. To know
more about its API, please refer to the `gnatcoll-json.ads
<https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-json.ads>`_
source file.

Streaming output
================

``GNATCOLL.JSON.Serializers`` emits compact JSON without constructing a
``JSON_Value`` tree. Extend the abstract limited type ``Serializer`` and
override
its ``Write`` primitive to consume UTF-8 output fragments. The sink can buffer
these fragments or write them to a stream. This complete example buffers a
nested object and array::

   with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
   with Ada.Text_IO;
   with GNATCOLL.JSON.Serializers;

   procedure JSON_Stream is
      package JSON renames GNATCOLL.JSON.Serializers;

      type Buffer_Serializer is new JSON.Serializer with record
         Output : Unbounded_String;
      end record;

      overriding procedure Write
        (Self : in out Buffer_Serializer; Content : String);

      overriding procedure Write
        (Self : in out Buffer_Serializer; Content : String) is
      begin
         Append (Self.Output, Content);
      end Write;

      W : Buffer_Serializer;
   begin
      W.Start_Object;
      W.Encode_Name ("sources");
      W.Start_Array;
      W.Encode_String ("main.adb");
      W.Encode_String ("foo.ads");
      W.End_Array;
      W.End_Object;
      W.Close;
      Ada.Text_IO.Put_Line (To_String (W.Output));
      --  {"sources":["main.adb","foo.ads"]}
   end JSON_Stream;

The serializer supplies punctuation and uses the same string escaping as
``JSON_Value.Write``. Its internal storage grows with nesting depth, rather
than
with the size of the document. A buffering sink, such as the example above,
still stores the complete output.

Use ``Encode_String`` for UTF-8 strings, ``Encode_Boolean`` for booleans,
``Encode_Number`` for integers or ``Long_Float`` values, and ``Encode_Null``
for
JSON null. ``Encode_String`` also accepts ``UTF8_Unbounded_String`` and
``UTF8_XString``. ``Encode_Number`` accepts ``Integer``, ``Long_Integer``, and
``Long_Long_Integer`` as well as ``Long_Float``.

``Encode_Name`` must precede each object member's value. Array elements are
written directly, without a name. Close each container with its matching
``End_Object`` or ``End_Array``. ``Close`` checks that the single root value is
complete; it does not close unfinished containers or flush the sink. Invalid
call sequences and non-finite numbers raise ``JSON_Serialization_Error``.

``Reset`` prepares the serializer for a new document and does not clear the
sink. Exceptions from ``Write`` propagate to the caller. If the sink raises an
exception, discard or reset the serializer before using it again, and handle
any partial output in the sink.
