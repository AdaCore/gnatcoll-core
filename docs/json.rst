****************************
**JSON**: handling JSON data
****************************

.. index:: json
.. highlight:: ada

JSON is a format often used on the web to communicate between
a server and a browser, or between servers. It plays a similar
role to XML, but is much lighter in terms of size. On the
other hand, it doesn't provide advanced features like validation
which XML provides.

The package **GNATCOLL.JSON** provides an Ada for creating
JSON data, or parse such data that your application receives.

Most JSON data will generally start with an object, on which
attributes can be set. The value for the attributes are also
JSON data.

Here is an example of use::

   pragma Ada_05;
   with GNATCOLL.JSON;   use GNATCOLL.JSON;
   with Ada.Text_IO;     use Ada.Text_IO;

   procedure JSON_Test is
      MyObj : JSON_Value := Create_Object;
   begin
      MyObj.Set_Field ("field1", Create (1));
      MyObj.Set_Field ("name", "theName");

      --  Now print the value
      Put_Line (MyObj.Write);
   end JSON_Test;

This example used the Ada05 dot notation to call the primitive
operations, but would also work using the more traditional
prefix notation.

It is also possible to create JSON arrays. These are not
tagged types, so the prefix notation has to be used. Here
is a further example that sets another field in the object
we had before::

  declare
     MyArr : JSON_Array := Empty_Array;
  begin
     Append (MyArr, Create (1));
     Append (MyArr, Create ("aString"));

     MyObj.Set_Field ("vals", MyArr);
  end;

GNATColl automatically takes care of memory management, and
all allocated memory is automatically freed when the object
is no longer needed.
