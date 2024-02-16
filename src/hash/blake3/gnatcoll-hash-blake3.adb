with System;
with Ada.Unchecked_Deallocation;

package body GNATCOLL.Hash.Blake3 is

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type size_t is mod 2 ** Standard'Address_Size;
   type Byte is mod 256;

   Hex_Digit : constant array (Byte range 0 .. 15) of Character :=
      "0123456789abcdef";

   procedure Blake3_Hasher_Finalize
      (H : System.Address; Buffer : System.Address; Size : size_t);
   pragma Import (C, Blake3_Hasher_Finalize, "blake3_hasher_finalize");

   ------------
   -- Blake3 --
   ------------

   function Blake3 (Str : String) return Ada.Containers.Hash_Type is
      Binary_Result : array (1 .. 8) of Ada.Containers.Hash_Type;
      Context : Blake3_Context;
   begin
      Init_Hash_Context (Context);
      Update_Hash_Context (Context, Str);

      Blake3_Hasher_Finalize
         (Context.Opaque (1)'Address,
          Binary_Result (Binary_Result'First)'Address, 32);
      return Binary_Result (1);
   end Blake3;

   ----------------------
   -- Blake3_File_Hash --
   ----------------------

   function Blake3_File_Hash
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return Blake3_Digest
   is
      FD      : FS.File_Descriptor;
      Context : Blake3_Context;
      N       : Integer;
      Buffer  : String_Access;
   begin
      Context.Init_Hash_Context;
      FD := FS.Open (Path => Path, Advise_Sequential => True);

      Buffer := new String (1 .. Buffer_Size);

      loop
         N := FS.Read (FD, Buffer.all);
         exit when N = 0;
         Context.Update_Hash_Context (Buffer (1 .. N));
      end loop;

      FS.Close (FD);
      Free (Buffer);
      return Context.Hash_Digest;
   end Blake3_File_Hash;

   -----------------
   -- Hash_Digest --
   -----------------

   function Hash_Digest
      (Self : in out Blake3_Context'Class) return Blake3_Digest
   is
      Binary_Result : array (1 .. 32) of Byte;
      Str_Result : Blake3_Digest;
   begin
      Blake3_Hasher_Finalize
         (Self.Opaque (1)'Address,
          Binary_Result (Binary_Result'First)'Address, 32);

      for Idx in Binary_Result'Range loop
         Str_Result (Idx * 2 - 1) := Hex_Digit (Binary_Result (Idx) / 16);
         Str_Result (Idx * 2) := Hex_Digit (Binary_Result (Idx) mod 16);
      end loop;

      return Str_Result;
   end Hash_Digest;

   -----------------------
   -- Init_Hash_Context --
   -----------------------

   procedure Init_Hash_Context (Self : in out Blake3_Context'Class) is
      procedure Blake3_Hasher_Init (A : System.Address);
      pragma Import (C, Blake3_Hasher_Init, "blake3_hasher_init");
   begin
      Blake3_Hasher_Init (Self.Opaque (1)'Address);
   end Init_Hash_Context;

   -------------------------
   -- Update_Hash_Context --
   -------------------------

   procedure Update_Hash_Context
      (Self : in out Blake3_Context'Class; Buffer : String)
   is
      procedure Blake3_Hasher_Update
         (H : System.Address; Buffer : System.Address; Size : size_t);
      pragma Import (C, Blake3_Hasher_Update, "blake3_hasher_update");
   begin
      if Buffer'Length > 0 then
         Blake3_Hasher_Update
            (Self.Opaque (1)'Address,
             Buffer (Buffer'First)'Address,
             Buffer'Length);
      end if;
   end Update_Hash_Context;

end GNATCOLL.Hash.Blake3;
