------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                      Copyright (C) 2020-2023, AdaCore                    --
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

with Interfaces.C;     use Interfaces.C;
with System;

package body GNATCOLL.OS.FS is

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
      function C_Close (FD : File_Descriptor) return int;
      pragma Import (C, C_Close, "close");

      Status : int;
      pragma Unreferenced (Status);
   begin
      Status := C_Close (FD);
   end Close;

   ----------------
   -- Is_Console --
   ----------------

   function Is_Console (FD : File_Descriptor) return Boolean is separate;

   ---------------
   -- Null_File --
   ---------------

   function Null_File return UTF8.UTF_8_String is separate;

   ----------
   -- Open --
   ----------

   function Open
      (Path : UTF8.UTF_8_String;
       Mode : Open_Mode := Read_Mode;
       Advise_Sequential : Boolean := False)
       return File_Descriptor
   is separate;

   ---------------
   -- Open_Pipe --
   ---------------

   procedure Open_Pipe
      (Pipe_Read  : out File_Descriptor;
       Pipe_Write : out File_Descriptor) is separate;

   ----------
   -- Read --
   ----------

   function Read (FD : File_Descriptor; Buffer : in out String) return Integer
   is
      function C_Read
        (Fd     : File_Descriptor;
         Buffer : System.Address;
         Size   : size_t)
         return int;
      pragma Import (C, C_Read, "read");

      Result : int;
   begin
      Result := C_Read (FD, Buffer (Buffer'First)'Address, Buffer'Length);
      if Result < 0 then
         raise OS_Error with "read error";
      end if;

      return Integer (Result);
   end Read;

   function Read
      (FD          : File_Descriptor;
       Buffer_Size : Positive := Default_Buffer_Size)
      return Unbounded_String
   is
      Buffer     : String (1 .. Buffer_Size);
      Bytes_Read : Integer;
      Result     : Unbounded_String;
   begin
      loop
         Bytes_Read := Read (FD, Buffer);
         exit when Bytes_Read = 0;

         Append (Result, Buffer (1 .. Bytes_Read));
      end loop;
      return Result;
   end Read;

   function Read
      (FD          : File_Descriptor;
       Buffer_Size : Positive := Default_Buffer_Size)
      return String
   is
      Result : Unbounded_String;
   begin
      Result := Read (FD, Buffer_Size);
      return To_String (Result);
   end Read;

   ----------------
   -- Read_Bytes --
   ----------------

   function Read_Bytes (FD : File_Descriptor) return T is
      Buffer     : String (1 .. T'Object_Size / 8);
      Byte_Reads : Integer;
   begin
      Byte_Reads := Read (FD, Buffer => Buffer);
      if Byte_Reads < T'Object_Size / 8 then
         raise OS_Error with "read error";
      end if;

      declare
         Result : T with Import, Convention => Ada, Address => Buffer'Address;
      begin
         return Result;
      end;
   end Read_Bytes;

   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean) is separate;

   -----------
   -- Write --
   -----------

   procedure Write (FD : File_Descriptor; Buffer : String)
   is
      Byte_Written : Integer;
   begin
      Byte_Written := Write (FD, Buffer);
      if Byte_Written < Buffer'Length then
         raise OS_Error with "write interrupted or not enough disk space";
      end if;
   end Write;

   function Write (FD : File_Descriptor; Buffer : String) return Integer
   is
      function C_Write
        (Fd     : File_Descriptor;
         Buffer : System.Address;
         Size   : size_t)
         return int;
      pragma Import (C, C_Write, "write");

      Result : int;
   begin
      Result := C_Write (FD, Buffer (Buffer'First)'Address, Buffer'Length);

      if Result < 0 then
         raise OS_Error with "write error";
      end if;

      return Integer (Result);
   end Write;

   -----------------
   -- Write_Bytes --
   -----------------

   procedure Write_Bytes (FD : File_Descriptor; Buffer : T) is
      function C_Write
        (Fd     : File_Descriptor;
         Buffer : System.Address;
         Size   : size_t)
         return int;
      pragma Import (C, C_Write, "write");

      Result : int;
   begin
      Result := C_Write (FD, Buffer'Address, T'Object_Size / 8);

      if Result /= T'Object_Size / 8 then
         raise OS_Error with "write error";
      end if;

   end Write_Bytes;
end GNATCOLL.OS.FS;
