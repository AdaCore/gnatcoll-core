------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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
with Ada.Unchecked_Deallocation;

package body GNATCOLL.Hash.xxHash is

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type size_t is mod 2 ** Standard'Address_Size;
   type byte is mod 256;
   Hex_Digit : constant array (byte range 0 .. 15) of Character :=
      "0123456789abcdef";

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out XXH3_Context) is
      function XXH3_FreeState (H : System.Address) return Integer;
      pragma Import (C, XXH3_FreeState, "XXH3_freeState");
      Status : Integer;
      pragma Unreferenced (Status);
   begin
      Status := XXH3_FreeState (Self.Opaque_State);
   end Finalize;

   -----------------
   -- Hash_Digest --
   -----------------

   function Hash_Digest (Self : in out XXH3_Context'Class) return XXH3_Hash is
      function XXH3_64bits_Digest (H : System.Address) return XXH3_Hash;
      pragma Import (C, XXH3_64bits_Digest, "XXH3_64bits_digest");
   begin
      return XXH3_64bits_Digest (Self.Opaque_State);
   end Hash_Digest;

   function Hash_Digest (Self : in out XXH3_Context'Class) return XXH3_Digest
   is
      Binary_Result : XXH3_Hash;
      Str_Result    : XXH3_Digest;
   begin
      Binary_Result := Hash_Digest (Self);

      for Idx in reverse Str_Result'Range loop
         Str_Result (Idx) := Hex_Digit (byte (Binary_Result mod 16));
         Binary_Result := Binary_Result / 16;
      end loop;
      return Str_Result;
   end Hash_Digest;

   -----------------------
   -- Init_Hash_Context --
   -----------------------

   procedure Init_Hash_Context (Self : in out XXH3_Context'Class) is

      function XXH3_createState return System.Address;
      pragma Import (C, XXH3_createState, "XXH3_createState");

      function XXH3_64bits_reset (S : System.Address) return Integer;
      pragma Import (C, XXH3_64bits_reset, "XXH3_64bits_reset");

      Status : Integer;
      pragma Unreferenced (Status);

   begin
      Self.Opaque_State := XXH3_createState;
      Status := XXH3_64bits_reset (Self.Opaque_State);
   end Init_Hash_Context;

   -------------------------
   -- Update_Hash_Context --
   -------------------------

   procedure Update_Hash_Context
      (Self : in out XXH3_Context'Class; Buffer : String)
   is
      function XXH3_64bits_Update
         (H      : System.Address;
          Buffer : System.Address;
          Size   : size_t)
         return Integer;
      pragma Import (C, XXH3_64bits_Update, "XXH3_64bits_update");

      Status : Integer;
      pragma Unreferenced (Status);
   begin
      if Buffer'Length > 0 then
         Status := XXH3_64bits_Update
            (Self.Opaque_State, Buffer (Buffer'First)'Address, Buffer'Length);
      end if;
   end Update_Hash_Context;

   ----------
   -- XXH3 --
   ----------

   function XXH3 (Buffer : String) return Ada.Containers.Hash_Type is
      function XXH3_64bits
         (Buffer : System.Address; Size : size_t) return XXH3_Hash;
      pragma Import (C, XXH3_64bits, "XXH3_64bits");
   begin
      return Ada.Containers.Hash_Type'Mod
         (XXH3_64bits (Buffer (Buffer'First)'Address, Buffer'Length));
   end XXH3;

   --------------------
   -- XXH3_File_Hash --
   --------------------

   function XXH3_File_Hash
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return XXH3_Digest
   is
      FD      : FS.File_Descriptor;
      Context : XXH3_Context;
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
   end XXH3_File_Hash;

end GNATCOLL.Hash.xxHash;
