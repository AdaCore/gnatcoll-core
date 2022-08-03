------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the impied warranty of MERCHAN- --
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

package body GNATCOLL.OS.FSUtil is

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   -------------
   -- Process --
   -------------

   function Process
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return Result_Type
   is
      FD      : FS.File_Descriptor;
      Context : State_Type := Initial_State;
      N       : Integer;
      Buffer  : String_Access;
   begin
      FD := FS.Open (Path => Path, Advise_Sequential => True);

      Buffer := new String (1 .. Buffer_Size);

      loop
         N := FS.Read (FD, Buffer.all);
         exit when N = 0;
         Update (Context, Buffer (1 .. N));
      end loop;

      FS.Close (FD);
      Free (Buffer);
      return Result (Context);
   end Process;

   -------------------
   -- Internal_SHA1 --
   -------------------

   function Internal_SHA1 is new Process
      (GNAT.SHA1.Context,
       SHA1_Digest,
       GNAT.SHA1.Initial_Context, GNAT.SHA1.Update, GNAT.SHA1.Digest);

   ---------------------
   -- Internal_SHA256 --
   ---------------------

   function Internal_SHA256 is new Process
      (GNAT.SHA256.Context,
       SHA256_Digest,
       GNAT.SHA256.Initial_Context, GNAT.SHA256.Update, GNAT.SHA256.Digest);

   ----------
   -- SHA1 --
   ----------
   function SHA1
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return SHA1_Digest renames Internal_SHA1;

   ------------
   -- SHA256 --
   ------------

   function SHA256
      (Path        : UTF8.UTF_8_String;
       Buffer_Size : Positive := FS.Default_Buffer_Size)
      return SHA256_Digest renames Internal_SHA256;

end GNATCOLL.OS.FSUtil;
