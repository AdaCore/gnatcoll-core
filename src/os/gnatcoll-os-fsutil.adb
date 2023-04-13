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
with GNATCOLL.OS.Stat;      use GNATCOLL.OS.Stat;
with GNATCOLL.OS.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNATCOLL.Strings_Impl;
with Ada.Characters.Handling;

package body GNATCOLL.OS.FSUtil is

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   function Copy_File_Content
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean;
   --  Copy file content. Return True on success.
   --  Raises a CONSTRAINT_ERROR exception if the source file
   --  length can not be contained in a SInt_64.

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

   -----------------
   -- Remove_File --
   -----------------

   function Remove_File (Path : UTF8.UTF_8_String) return Boolean is separate;

   ------------------
   -- Copy_Content --
   ------------------

   function Copy_File_Content
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String)
      return Boolean is separate;

   ---------------
   -- Copy_File --
   ---------------

   function Copy_File
     (Src                  : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String;
      Preserve_Timestamps  : Boolean := False;
      Preserve_Permissions : Boolean := False) return Boolean
   is
      Src_File_Attr : File_Attributes;

   begin
      Src_File_Attr := GNATCOLL.OS.Stat.Stat (Src);
      if not Exists (Src_File_Attr) or else not Is_File (Src_File_Attr) then
         --  File does not exist, or is not a regular file.
         return False;
      end if;

      if not Copy_File_Content (Src, Dst) then
         return False;
      end if;

      if Preserve_Permissions then
         if not Copy_Permissions (Src, Dst) then
            return False;
         end if;
      end if;

      if Preserve_Timestamps then
         if not Copy_Timestamps (Src, Dst) then
            return False;
         end if;
      end if;

      return True;
   end Copy_File;

   ---------------------
   -- Copy_Timestamps --
   ---------------------

   function Copy_Timestamps
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String)
      return Boolean is separate;

   ----------------------
   -- Copy_Permissions --
   ----------------------

   function Copy_Permissions
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String)
      return Boolean is separate;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Path : UTF8.UTF_8_String) return Boolean is separate;

   --------------------------
   -- Create_Symbolic_Link --
   --------------------------

   function Create_Symbolic_Link
     (Link_Path : UTF8.UTF_8_String; Target_Path : UTF8.UTF_8_String)
      return Boolean is separate;

   ----------------------
   -- Remove_Directory --
   ----------------------

   function Remove_Directory
     (Path : UTF8.UTF_8_String) return Boolean is separate;

   ------------------------
   -- Read_Symbolic_Link --
   ------------------------

   function Read_Symbolic_Link
     (Link_Path : UTF8.UTF_8_String; Target_Path : out Unbounded_String)
      return Boolean is separate;

   -------------------------------
   -- Symbolic_Link_Is_Internal --
   -------------------------------

   function Symbolic_Link_Is_Internal
     (Top_Dir_Path : UTF8.UTF_8_String; Link_Path : UTF8.UTF_8_String)
      return Boolean
   is
      Target_Path : Unbounded_String;

   begin

      if not Read_Symbolic_Link (Link_Path, Target_Path) then
         return False;
      end if;

      if GNAT.OS_Lib.Is_Absolute_Path (To_String (Target_Path)) then
         return False;
      end if;

      --  Concatenate Link_Path and Target_Path split by split, and see if
      --  it goes outside Top_Dir.

      --  Example top dir: /tmp/dir and target_path: dir1/../../dir/fileA
      --  In order:
      --  - /tmp/dir/dir1: OK
      --  - /tmp/dir/dir1/..: OK
      --  - /tmp/dir/dir1/../..: KO!

      declare
         use Ada.Characters.Handling;
         package Strings is new GNATCOLL.Strings_Impl.Strings
           (GNATCOLL.Strings_Impl.Optimal_String_Size, Character, String);
         use Strings;

         package Constants renames GNATCOLL.OS.Constants;

         function Normalize_Link_Pathname (Link_Path : String) return String;
         --  Obtain link normalized pathname without resolving the link itself

         function Normalize_Link_Pathname (Link_Path : String) return String is
            Normalized_Link_Path : constant String :=
              GNAT.OS_Lib.Normalize_Pathname
                (Link_Path, Resolve_Links => False);
            X                    : Strings.XString;
         begin
            X.Set (Normalized_Link_Path);

            declare
               X_Arr : constant Strings.XString_Array :=
                 X.Right_Split
                   (Sep        => Constants.Dir_Sep, Max_Split => 2,
                    Omit_Empty => True);
               --  Extract symbolic link basename, and base path
            begin

               if X_Arr'Length = 1 then

                  --  Can occur if the link is at the top of a file system.

                  return Constants.Dir_Sep & X_Arr (1).To_String;
               else

                  --  Resolve symbolic links if needed, and normalize path
                  --  to final link directory.

                  return
                    GNAT.OS_Lib.Normalize_Pathname (X_Arr (2).To_String) &
                    Constants.Dir_Sep & X_Arr (1).To_String;
               end if;
            end;
         end Normalize_Link_Pathname;

         Normalized_Top_Dir   : constant String :=
           GNAT.OS_Lib.Normalize_Pathname (Top_Dir_Path);
         Normalized_Link_Path : constant String :=
           Normalize_Link_Pathname (Link_Path);
         Link_Depth           : Integer         := 0;
         XS                   : Strings.XString;
      begin

         --  Check if the link is outside the top directory
         --  First case: Normalized link path is simply shorter than
         --  the top directory one.
         --  Second case: Normalized top directory path is not contained in
         --  normalized link path.

         if Normalized_Link_Path'Length < Normalized_Top_Dir'Length
           or else
             Normalized_Link_Path
               (Normalized_Link_Path'First ..
                    Normalized_Link_Path'First + Normalized_Top_Dir'Length -
                    1) /=
             Normalized_Top_Dir
         then
            return False;
         end if;

         --  Compute the link depth in the top directory

         XS.Set
           (Normalized_Link_Path
              (Normalized_Top_Dir'Last + 1 .. Normalized_Link_Path'Last));

         Link_Depth := 0;

         declare
            First_Item : Boolean := True;
         begin
            for Dir_Entry of XS.Split
              (GNATCOLL.OS.Constants.Dir_Sep, Omit_Empty => True)
            loop
               if First_Item then
                  First_Item := False;

                  --  The last XS.Split element is the link itself.
                  --  It shall not count in depth.

               else
                  Link_Depth := Link_Depth + 1;
               end if;
            end loop;
         end;

         --  Then, concatenate each subpart of the target path relative to the
         --  link, and see if depth becomes negative once. In this case, the
         --  target link went outside of the directory.

         XS.Set (To_String (Target_Path));
         for Dir_Entry of XS.Split
           (GNATCOLL.OS.Constants.Dir_Sep, Omit_Empty => True)
         loop
            if Dir_Entry = ".." then
               Link_Depth := Link_Depth - 1;
            elsif Dir_Entry /= "." then
               Link_Depth := Link_Depth + 1;
            end if;

            if Link_Depth < 0 then
               return False;
            end if;
         end loop;
      end;

      return True;
   end Symbolic_Link_Is_Internal;

end GNATCOLL.OS.FSUtil;
