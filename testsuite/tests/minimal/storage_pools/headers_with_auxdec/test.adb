--  Very simple test of GNATCOLL.Storage_Pools.Headers checking that it is
--  useable together with DEC system extensions (S507-006).

pragma Extend_System (Aux_DEC);

with GNATCOLL.Storage_Pools.Headers;  use GNATCOLL.Storage_Pools.Headers;
with Ada.Text_IO;                     use Ada.Text_IO;

with System.Address_Image;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;

   type Header is record
      Refcount : Natural := 0;
   end record;
   type Header_Access is access all Header;

   package Pools is new Header_Pools (Header, Header_Access);

   package String_Pools is new Pools.Typed (String);

   Str : String_Pools.Element_Access;
   Hdr : Header_Access;
begin
   --  Allocate element from the typed storage pool then retrieve
   --  and access its header

   Str := new String'("foo");
   Put_Line (System.Address_Image (Str.all'Address));

   Hdr := String_Pools.Header_Of (Str);
   Put_Line (System.Address_Image (Hdr.all'Address));

   String_Pools.Header_Of (Str).all.Refcount := 1;

   A.Assert (Hdr.all.Refcount = 1, "unexpected ref count from extra header");

   return A.Report;
end Test;
