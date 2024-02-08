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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with GNATCOLL.OS.Stat;      use GNATCOLL.OS.Stat;
with GNATCOLL.OS.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNATCOLL.Strings_Impl;
with Ada.Characters.Handling;
with GNATCOLL.OS.Dir;
with Ada.Calendar;
with Ada.Assertions;

package body GNATCOLL.OS.FSUtil is

   type String_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   Sync_Trees_Exception : exception;

   function Copy_File_Content
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String) return Boolean;
   --  Copy file content. Return True on success.
   --  Raises a CONSTRAINT_ERROR exception if the source file
   --  length can not be contained in a SInt_64.

   procedure Sync_Trees_Internal
     (Src          : UTF8.UTF_8_String;
      Dst          : UTF8.UTF_8_String;
      Mode         : Sync_Trees_Mode;
      Symlink_Mode : Sync_Trees_Symlink_Mode := SKIP_SYMLINKS);
   --  Internal sync function doing all the work.
   --  This is not done directly in Sync_Trees so exceptions can be handled.

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

   ------------------------
   -- Copy_Symbolic_Link --
   ------------------------

   function Copy_Symbolic_Link
     (Src_Path : UTF8.UTF_8_String; Dst_Path : UTF8.UTF_8_String)
      return Boolean
   is
      Target_Path : Unbounded_String;
   begin

      if not Read_Symbolic_Link (Src_Path, Target_Path) then
         return False;
      end if;

      return Create_Symbolic_Link
               (Link_Path   => Dst_Path,
                Target_Path => To_String (Target_Path));
   end Copy_Symbolic_Link;


   ----------------
   -- Sync_Trees --
   ----------------

   function Sync_Trees
     (Src          : UTF8.UTF_8_String;
      Dst          : UTF8.UTF_8_String;
      Mode         : Sync_Trees_Mode;
      Symlink_Mode : Sync_Trees_Symlink_Mode := SKIP_SYMLINKS)
      return Boolean
   is
   begin
      Sync_Trees_Internal (Src, Dst, Mode, Symlink_Mode);

      --  No exception detected

      return True;
   exception
      when E : Sync_Trees_Exception =>
         Ada.Text_IO.Put_Line
           ("Sync_Trees from " & Src & " to " & Dst & " failed:");
         Ada.Text_IO.Put_Line (Exception_Message (E));
         return False;
   end Sync_Trees;

   -------------------------
   -- Sync_Trees_Internal --
   -------------------------

   procedure Sync_Trees_Internal
     (Src          : UTF8.UTF_8_String;
      Dst          : UTF8.UTF_8_String;
      Mode         : Sync_Trees_Mode;
      Symlink_Mode : Sync_Trees_Symlink_Mode := SKIP_SYMLINKS)
   is

      use GNATCOLL.OS.Dir;
      use Ada.Calendar;

      Absolute_Src : Unbounded_String;
      Absolute_Dst : Unbounded_String;

      function Dir_Absolute_Path
        (Path : UTF8.UTF_8_String) return UTF8.UTF_8_String;
      --  Return directory absolute path.

      procedure Clean_Dst;
      --  Remove files which are in the destination tree, but not in the source
      --  one. Checks only for names, no matter the type of the file.

      procedure Sync_Trees_Src_To_Dst;
      --  Copy source tree content to the destination one

      function Dir_Absolute_Path
        (Path : UTF8.UTF_8_String) return UTF8.UTF_8_String
      is
         DH : Dir_Handle                 := GNATCOLL.OS.Dir.Open (Path);
         P  : constant UTF8.UTF_8_String := GNATCOLL.OS.Dir.Path (DH);
      begin
         Close (DH);
         return P;
      end Dir_Absolute_Path;

      procedure Clean_Dst is

         function Get_Src_Path
           (H : Dir_Handle; Element : Dir_Entry) return UTF8.UTF_8_String;
         --  Return directory entry path based on Sync_Trees source
         --  base directory.

         function Get_Src_Path
           (H : Dir_Handle; Element : Dir_Entry) return UTF8.UTF_8_String
         is
            Dir_Entry_Path : constant UTF8.UTF_8_String := Path (H, Element);

            --  Take the part of complete dir entry path which is not in
            --  the Sync_Trees top source path => the right part of the path,
            --  and concatenate it with the Sync_Trees top destination path.

            Result : constant UTF8.UTF_8_String :=
              To_String (Absolute_Src) &
              Dir_Entry_Path
                (To_String (Absolute_Dst)'Last + 1 .. Dir_Entry_Path'Last);
         begin
            return Result;
         end Get_Src_Path;

         function Handle_Dir
           (H : Dir_Handle; Element : Dir_Entry) return Boolean;

         function Handle_Dir
           (H : Dir_Handle; Element : Dir_Entry) return Boolean
         is
            Src_Path : constant UTF8.UTF_8_String := Get_Src_Path (H, Element);
            Dst_Path : constant UTF8.UTF_8_String := Path (H, Element);
            Src_FA   : constant Stat.File_Attributes :=
              Stat.Stat (Src_Path, Follow_Symlinks => False);
         begin

            --  If directory does not exist in the source path,
            --  then delete it in the destination path.

            if not Stat.Exists (Src_FA) then
               if not Remove_Directory (Dst_Path) then
                  raise Sync_Trees_Exception
                    with "Failed to remove the directory " & Dst_Path;
               end if;
            end if;
            return True;
         end Handle_Dir;

         procedure Handle_File (H : Dir_Handle; Element : Dir_Entry);

         procedure Handle_File (H : Dir_Handle; Element : Dir_Entry) is
            Src_Path : constant UTF8.UTF_8_String := Get_Src_Path (H, Element);
            Dst_Path : constant UTF8.UTF_8_String := Path (H, Element);
            Src_FA   : constant Stat.File_Attributes :=
              Stat.Stat (Src_Path, Follow_Symlinks => False);
         begin

            --  If file does not exist in the source path, then delete it from
            --  destination path.

            if not Stat.Exists (Src_FA) then
               if not Remove_File (Dst_Path) then
                  raise Sync_Trees_Exception
                    with "Failed to remove the file " & Dst_Path;
               end if;
            end if;
         end Handle_File;

      begin

         --  Do not follow symlinks, as we do not want to check the symbolic
         --  link targets.

         Walk
           (Path                 => Dst,
            File_Handler         => Handle_File'Unrestricted_Access,
            Dir_Handler          => Handle_Dir'Unrestricted_Access,
            Follow_Symlinks      => False,
            Propagate_Exceptions => True);
      end Clean_Dst;

      procedure Sync_Trees_Src_To_Dst is

         function Equals
           (First_Path  : UTF8.UTF_8_String;
            First_FA    : Stat.File_Attributes;
            Second_Path : UTF8.UTF_8_String;
            Second_FA   : Stat.File_Attributes)
            return Boolean;
         --  Compare two files or directories. Checksum comparison mode can be
         --  used only for regular files. Timestamps and length comparison
         --  is applied otherwise.
         --
         --  Return true if entities are equals.

         procedure Handle_File (H : Dir_Handle; Element : Dir_Entry);
         --  Ensure that the file currently handled is also present in the
         --  destination directory.

         function Handle_Dir
           (H : Dir_Handle; Element : Dir_Entry) return Boolean;
         --  Ensure that the directory currently handled is also present in
         --  the destination directory. The content directory is not processed,
         --  as it is processed by the Walk's call of Sync_Trees_Src_To_Dst.

         function Get_Dst_Path
           (H : Dir_Handle; Element : Dir_Entry) return UTF8.UTF_8_String;
         --  Return directory entry path based on Sync_Trees destination
         --  base directory.

         procedure Remove_If_Exists (Path : UTF8.UTF_8_String);
         --  Remove file or directory if it exists.
         --  Raise Sync_Trees_Exception on error.

         procedure Process_Symbolic_Link
           (Src_Path  :     UTF8.UTF_8_String; Dst_Path : UTF8.UTF_8_String;
            Processed : out Boolean);
         --   Copy symbolic link if the symlink mode and the symbolic link
         --   target location (internal / external) allows it.
         --   A symbolic link can be processed whithout being copied,
         --   this is for example the case if the link target is an absolute
         --   path and if symlink mode is set to copy_safe_symlinks.
         --   - The Processed boolean indicates if symbolic link has been
         --     processed. A False value means that the symbolic link target
         --     shall be processed instead of the symbolic link itself.
         --
         --   Raise Sync_Trees_Exception on error.

         function Equals
           (First_Path  : UTF8.UTF_8_String; First_FA : Stat.File_Attributes;
            Second_Path : UTF8.UTF_8_String; Second_FA : Stat.File_Attributes)
            return Boolean
         is
         begin
            if Stat.Length (First_FA) /= Stat.Length (Second_FA) then
               return False;
            end if;

            --  Checksum verification is viable only for files

            if Mode = CHECKSUM and Stat.Is_File (First_FA) and
              Stat.Is_File (Second_FA)
            then
               return SHA1 (First_Path) = SHA1 (Second_Path);
            end if;

            --  ??? Add permission checking

            return
              Stat.Modification_Time (First_FA) =
              Stat.Modification_Time (Second_FA);
         end Equals;

         function Get_Dst_Path
           (H : Dir_Handle; Element : Dir_Entry) return UTF8.UTF_8_String
         is
            Dir_Entry_Path : constant UTF8.UTF_8_String := Path (H, Element);

            Result : constant UTF8.UTF_8_String :=
              To_String (Absolute_Dst) &
              Dir_Entry_Path
                (To_String (Absolute_Src)'Last + 1 .. Dir_Entry_Path'Last);

            --  Take the part of complete dir entry path which is not in
            --  the Sync_Trees top source path => the right part of the path,
            --  and concatenate it with the Sync_Trees top destination path.

         begin
            return Result;
         end Get_Dst_Path;

         procedure Remove_If_Exists (Path : UTF8.UTF_8_String) is
            FA : constant Stat.File_Attributes :=
              Stat.Stat (Path, Follow_Symlinks => False);
         begin
            if Stat.Exists (FA) then
               if Stat.Is_Directory (FA) then
                  if not Remove_Directory (Path) then
                     raise Sync_Trees_Exception
                       with "Failed to remove the directory " & Path;
                  end if;
               else
                  if not Remove_File (Path) then
                     raise Sync_Trees_Exception
                       with "Failed to remove the file " & Path;
                  end if;
               end if;
            end if;
         end Remove_If_Exists;

         procedure Process_Symbolic_Link
           (Src_Path  : UTF8.UTF_8_String;
            Dst_Path  : UTF8.UTF_8_String;
            Processed : out Boolean)
         is
         begin
            case Symlink_Mode is
               when SKIP_SYMLINKS =>
                  Processed := True;

                  --  Do not process symlink at all. Destination directory
                  --  shall not have any symlink after cleaning. As cleaning
                  --  is done only based on name, we need to clean this link
                  --  manually.

                  Remove_If_Exists (Dst_Path);
               when COPY_SYMLINKS =>
                  Processed := True;

                  --  Copy symlink without target path modification

                  Remove_If_Exists (Dst_Path);

                  if not Copy_Symbolic_Link (Src_Path, Dst_Path) then
                     raise Sync_Trees_Exception with
                       "Failed to copy symbolic link from "
                       & Src_Path & " to " & Dst_Path;
                  end if;
               when COPY_SAFE_SYMLINKS =>
                  Processed := True;

                  --  If the link is external, then dst directory shall not
                  --  contain entity of the same name. If the link is internal,
                  --  then it is copied to dst.

                  Remove_If_Exists (Dst_Path);

                  if Symbolic_Link_Is_Internal
                       (Top_Dir_Path => To_String (Absolute_Src),
                        Link_Path    => Src_Path)
                  then
                     if not Copy_Symbolic_Link (Src_Path, Dst_Path) then
                        raise Sync_Trees_Exception with
                          "Failed to copy symbolic link from "
                          & Src_Path & " to " & Dst_Path;
                     end if;
                  end if;
               when COPY_UNSAFE_SYMLINKS =>

                  --  Only internal symbolic links should be processed
                  --  as symlinks. External symbolic links have
                  --  their target copied instead.

                  if Symbolic_Link_Is_Internal
                       (Top_Dir_Path => To_String (Absolute_Src),
                        Link_Path    => Src_Path)
                  then
                     Processed := True;

                     Remove_If_Exists (Dst_Path);
                     if not (Copy_Symbolic_Link (Src_Path, Dst_Path))
                     then
                        raise Sync_Trees_Exception with
                          "Failed to copy symbolic link from "
                          & Src_Path & " to " & Dst_Path;
                     end if;
                  else
                     Processed := False;
                  end if;
               when COPY_SYMLINKS_TARGET =>

                  --  Copy symlinks target instead of the symlink itself.
                  --  Should be recursive for symbolic link targeting a
                  --  directory.

                  Processed := False;
            end case;
         end Process_Symbolic_Link;

         function Handle_Dir
           (H : Dir_Handle; Element : Dir_Entry) return Boolean
         is
            Src_Path : constant UTF8.UTF_8_String := Path (H, Element);
            Dst_Path : constant UTF8.UTF_8_String := Get_Dst_Path (H, Element);
            Src_FA   : constant Stat.File_Attributes := Attributes (Element);

         begin
            declare
               Processed : Boolean;
               Src_LFA   : constant Stat.File_Attributes :=
                 Stat.Stat (Src_Path, Follow_Symlinks => False);
            begin
               if Stat.Is_Symbolic_Link (Src_LFA) then
                  Process_Symbolic_Link (Src_Path, Dst_Path, Processed);

                  if Processed then

                     --  Symbolic link has been processed. We do not browse
                     --  symbolic link, as either the link target is
                     --  internal and the target will be browsed with Walk,
                     --  or the link is external and it shall not be
                     --  processed at all.

                     return False;
                  end if;

                  --  In this case, symbolic link has not been processed,
                  --  and its target shall be processed instead.

               end if;
            end;

            --  At this stage of the code, source either is not a
            --  symbolic link or shall be processed as a regular directory.

            declare

               Dst_LFA : constant Stat.File_Attributes :=
                 Stat.Stat (Dst_Path, Follow_Symlinks => False);
            begin

               --  In contrary to Handle_File, a symbolic link target can not
               --  be invalid here. A symbolic link is seen as a directory only
               --  if the target is a directory. If not, a file is detected
               --  instead, so we would go through the Handle_File handler.

               if Stat.Is_Directory (Dst_LFA) then

                  --  If destination directory already exists, compare src
                  --  and dst to check if timestamp and permission copy
                  --  is required.

                  if not Equals (Src_Path, Src_FA, Dst_Path, Dst_LFA) then
                     if not Copy_Timestamps (Src_Path, Dst_Path) then
                        raise Sync_Trees_Exception with
                          "Failed to copy timestamp from directory " & Src_Path
                          & " to directory " & Dst_Path;
                     end if;
                  end if;
               else

                  --  If the destination already exists as a regular file,
                  --  the latter must be deleted, and replaced by the
                  --  directory.

                  if Stat.Exists (Dst_LFA) then

                     --  Remove symbolic link or regular file if it exists

                     if not Remove_File (Dst_Path) then
                        raise Sync_Trees_Exception with
                          "Failed to remove file " & Dst_Path;
                     end if;
                  end if;

                  if not Create_Directory (Dst_Path) then
                     raise Sync_Trees_Exception with
                          "Failed to create directory " & Dst_Path;
                  end if;

                  if not Copy_Timestamps (Src_Path, Dst_Path) then
                     raise Sync_Trees_Exception with
                          "Failed to copy timestamps from " & Src_Path & " to "
                          & Dst_Path;
                  end if;
               end if;
            end;

            return True;
         end Handle_Dir;

         procedure Handle_File (H : Dir_Handle; Element : Dir_Entry) is
            Src_Path : constant UTF8.UTF_8_String := Path (H, Element);
            Dst_Path : constant UTF8.UTF_8_String := Get_Dst_Path (H, Element);
            Src_FA   : constant Stat.File_Attributes := Attributes (Element);

         begin
            declare
               Src_LFA   : constant Stat.File_Attributes :=
                 Stat.Stat (Src_Path, Follow_Symlinks => False);
               Processed : Boolean;
            begin
               if Stat.Is_Symbolic_Link (Src_LFA) then
                  Process_Symbolic_Link (Src_Path, Dst_Path, Processed);

                  if Processed then
                     return;
                  end if;

                  --  In this case, symbolic link has not been processed,
                  --  and its target shall be processed instead.

               end if;
            end;

            --  At this stage of the code, source either is either not a
            --  symbolic link or its target should be processed instead.

            declare
               Dst_LFA : constant Stat.File_Attributes :=
                 Stat.Stat (Dst_Path, Follow_Symlinks => False);
            begin

               if not Stat.Exists (Src_FA) then

                  --  Can happen if a symbolic link has an invalid target

                  Ada.Assertions.Assert
                    (Symlink_Mode = COPY_UNSAFE_SYMLINKS or else
                     Symlink_Mode = COPY_SYMLINKS_TARGET);

                  --  Having an invalid target is not compatible with the
                  --  Sync_Trees modes COPY_UNSAFE_SYMLINKS and
                  --  COPY_SYMLINKS_TARGET, as the symbolic link's target needs
                  --  to be copied instead of the symbolic link."

                  raise Sync_Trees_Exception with
                    "Symbolic link " & Src_Path & " has an invalid target.";

               elsif Stat.Is_File (Dst_LFA) then
                  if not Equals (Src_Path, Src_FA, Dst_Path, Dst_LFA) then
                     if not Copy_File
                         (Src_Path, Dst_Path, Preserve_Timestamps => True,
                          Preserve_Permissions => False)
                     then
                        raise Sync_Trees_Exception
                          with "Failed to copy file from " & Src_Path
                            & " to " & Dst_Path;
                     end if;
                  end if;
               else
                  --  Remove dst symbolic link or directory if it exists

                  if Stat.Is_Directory (Dst_LFA) then
                     if not Remove_Directory (Dst_Path) then
                        raise Sync_Trees_Exception
                          with "Failed to remove the directory " & Dst_Path;
                     end if;
                  elsif Stat.Is_Symbolic_Link (Dst_LFA) then
                     if not Remove_File (Dst_Path) then
                        raise Sync_Trees_Exception
                          with "Failed to remove the file " & Dst_Path;
                     end if;
                  end if;

                  if not Copy_File
                      (Src_Path, Dst_Path, Preserve_Timestamps => True,
                       Preserve_Permissions => False)
                  then
                     raise Sync_Trees_Exception
                          with "Failed to copy the file " & Src_Path
                          & " to " & Dst_Path;
                  end if;
               end if;
            end;
         end Handle_File;

      begin

         --  We must follow symlinks. If a link targets a directory which
         --  should be copied because of the specified Sync_Trees symlink mode,
         --  then targeted directory must be also browsed by Walk. It could
         --  be done with Follow_Symlinks set to False with new call to Walk,
         --  but we would lose the initial Max_Depth Walk property, which
         --  prevents symlinks loops.

         Walk
           (Path                 => Src,
            File_Handler         => Handle_File'Unrestricted_Access,
            Dir_Handler          => Handle_Dir'Unrestricted_Access,
            Follow_Symlinks      => True,
            Propagate_Exceptions => True);
      end Sync_Trees_Src_To_Dst;

   begin
      declare
         Src_FA : constant File_Attributes :=
            Stat.Stat (Path => Src, Follow_Symlinks => True);
         Dst_FA : constant File_Attributes :=
                    Stat.Stat (Path => Dst, Follow_Symlinks => True);
      begin
         if not Stat.Exists (Src_FA) then
            raise Sync_Trees_Exception with "Source " & Src & " does not exist";
         elsif not Stat.Is_Directory (Src_FA) then
            raise Sync_Trees_Exception with
              "Source" & Src & " should be a directory";
         end if;

         if not Stat.Exists (Dst_FA) then
            declare

               --  Check that dst is not a symbolic link whose target does not
               --  exist.

               Dst_LFA : constant File_Attributes :=
                 Stat.Stat (Path => Dst, Follow_Symlinks => False);
            begin
               if Stat.Exists (Dst_LFA) and then
                  Stat.Is_Symbolic_Link (Dst_LFA)
               then
                  raise Sync_Trees_Exception with
                    Dst & " is an invalid symbolic link. Aborting the sync";
               end if;
            end;

            if not Create_Directory (Dst) then
               raise Sync_Trees_Exception with
                 "Failed to create the destination "
                 & "directory " & Dst;
            end if;
         elsif Stat.Is_File (Dst_FA) then
            raise Sync_Trees_Exception with
              Dst & " is a regular file. Only syncs between directories are "
              & "supported";
         end if;
      end;

      Absolute_Src := To_Unbounded_String (Dir_Absolute_Path (Src));
      Absolute_Dst := To_Unbounded_String (Dir_Absolute_Path (Dst));

      --  Remove file which are only in the destination tree.
      --  This is done before the synchronization, as the latter will
      --  do nothing in the best case, and will add new files otherwise.
      --  As cleaning implies tree browsing, it is better to do it on as few
      --  files as possible.

      Clean_Dst;

      --  Copy files from source to dest

      Sync_Trees_Src_To_Dst;
   end Sync_Trees_Internal;

end GNATCOLL.OS.FSUtil;
