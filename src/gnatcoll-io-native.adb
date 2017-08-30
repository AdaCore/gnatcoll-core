------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with System;
with Ada.Unchecked_Deallocation;
with Ada.Directories;

with Ada.Calendar.Formatting;  use Ada.Calendar;
with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;
with Interfaces.C;              use Interfaces.C;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNATCOLL.Mmap;
with GNATCOLL.Path;             use GNATCOLL.Path;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

package body GNATCOLL.IO.Native is

   package body Codec is separate;
   --  Separate implementation for Windows and Unix

   ------------
   -- Create --
   ------------

   function Create (Path : FS_String) return File_Access is
   begin
      return new Native_File_Record'
        (Ref_Count  => 1,
         Full       => new FS_String'(From_Unix (Local_FS, Path)),
         Normalized => null,
         Normalized_And_Resolved => null,
         Kind       => Unknown);
   end Create;

   ------------------------
   -- Dispatching_Create --
   ------------------------

   function Dispatching_Create
     (Ref       : not null access Native_File_Record;
      Full_Path : FS_String) return File_Access
   is
      pragma Unreferenced (Ref);
   begin
      return Create (Full_Path);
   end Dispatching_Create;

   -------------
   -- To_UTF8 --
   -------------

   function To_UTF8
     (Ref  : not null access Native_File_Record;
      Path : FS_String) return String
   is
      pragma Unreferenced (Ref);
   begin
      return Codec.To_UTF8 (Path);
   end To_UTF8;

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8
     (Ref  : not null access Native_File_Record;
      Path : String) return FS_String
   is
      pragma Unreferenced (Ref);
   begin
      return Codec.From_UTF8 (Path);
   end From_UTF8;

   -----------------
   -- Current_Dir --
   -----------------

   function Current_Dir return File_Access is
      D   : constant GNAT.Directory_Operations.Dir_Name_Str :=
              GNAT.Directory_Operations.Get_Current_Dir;
      Ret : File_Access;
   begin
      Ret := Create (FS_String (D));
      return Ret;
   end Current_Dir;

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir return File_Access is
      HOME : GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv ("HOME");
      Tmp  : GNAT.Strings.String_Access;
   begin
      if HOME.all = "" then
         Free (HOME);
         HOME := GNAT.OS_Lib.Getenv ("USERPROFILE");

         if HOME.all = "" then
            Free (HOME);
            return Create (Get_Root (Local_FS, ""));
         end if;
      end if;

      if HOME'Length > 2
        and then HOME (HOME'First) = '%'
        and then HOME (HOME'Last) = '%'
      then
         --  Some Windows systems set %HOME% to another env variable, e.g.
         --  %USERPROFILE%

         Tmp := HOME;
         HOME := GNAT.OS_Lib.Getenv (Tmp (Tmp'First + 1 .. Tmp'Last - 1));
         Free (Tmp);
      end if;

      declare
         --  ??? Convert from display charset to filesystem charset ?
         Result : constant FS_String :=
                    From_Unix (Local_FS, FS_String (HOME.all));
      begin
         Free (HOME);
         return Create (Result);
      end;
   end Home_Dir;

   -----------------------
   -- Get_Tmp_Directory --
   -----------------------

   function Get_Tmp_Directory return File_Access is

      function Internal return chars_ptr;
      pragma Import (C, Internal, "__gnatcoll_get_tmp_dir");

      procedure c_free (C : chars_ptr);
      pragma Import (C, c_free, "free");

      C_Str : constant chars_ptr := Internal;
      Str   : constant FS_String :=
                FS_String
                  (GNAT.Directory_Operations.Format_Pathname
                     (To_Ada (Value (C_Str))));
   begin
      --  Since the allocation was done in C (strdup), we use directly the
      --  C version of free. This is probably more reliable, and more
      --  importantly, this works correctly with our own version of
      --  s-memory.adb (when GPS_MEMORY_MONITOR=1)
      c_free (C_Str);
      return Create (Ensure_Directory (Local_FS, Str));
   end Get_Tmp_Directory;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   function Get_Logical_Drives return File_Array is

      function Internal
        (Buffer : System.Address;
         Length : Integer) return Integer;
      pragma Import (C, Internal, "__gnatcoll_get_logical_drive_strings");
      Len    : Integer;
      Last   : Natural;
      N      : Natural;

   begin
      --  First get the size of the buffer needed to contain the drives
      Len := Internal (System.Null_Address, 0);

      if Len = 0 then
         return (1 .. 0 => <>);
      end if;

      declare
         --  Use the returned length for creating the buffer. Do not forget
         --  to add room for the trailing \n
         Buffer : aliased FS_String (1 .. Len + 1);
      begin
         Len := Internal (Buffer'Address, Len);

         N := 0;
         for J in 1 .. Len loop
            if Buffer (J) = ASCII.NUL then
               N := N + 1;
            end if;
         end loop;

         declare
            Ret : File_Array (1 .. N);
         begin
            N := 1;
            Last := Buffer'First;

            for J in 1 .. Len loop
               if Buffer (J) = ASCII.NUL then
                  Ret (N) := Create
                    (GNATCOLL.Path.Path
                       (Local_FS, Buffer (Last .. Last), "", ""));
                  N := N + 1;
                  Last := J + 1;
               end if;
            end loop;

            return Ret;
         end;
      end;
   end Get_Logical_Drives;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (File : Native_File_Record) return Boolean is
      pragma Unreferenced (File);
   begin
      return True;
   end Is_Local;

   ------------
   -- Get_FS --
   ------------

   function Get_FS
     (File : not null access Native_File_Record) return FS_Type
   is
      pragma Unreferenced (File);
   begin
      return Local_FS;
   end Get_FS;

   ----------------------
   -- Resolve_Symlinks --
   ----------------------

   procedure Resolve_Symlinks
     (File : not null access Native_File_Record)
   is
      Is_Dir_Path : Boolean;
   begin
      if File.Normalized_And_Resolved = null then
         Is_Dir_Path := Path.Is_Dir_Name (File.Get_FS, File.Full.all);

         declare
            --  We have to pass "" for the directory, in case File.Full is a
            --  relative path name. That might be surprising to the application
            --  since the current directory might have changed since File was
            --  created.
            Norm : constant String :=
                     GNAT.OS_Lib.Normalize_Pathname
                       (String (File.Full.all),
                        Directory     => "",
                        Resolve_Links => True);
         begin
            --  Normalize_Pathname sometimes removes the trailing dir separator
            --  We need to take care of it then.
            if not Is_Dir_Path
              or else Norm (Norm'Last) = GNAT.OS_Lib.Directory_Separator
            then
               if File.Normalized /= null
                 and then FS_String (Norm) = File.Normalized.all
               then
                  File.Normalized_And_Resolved := File.Normalized;
               else
                  File.Normalized_And_Resolved :=
                    new FS_String'(FS_String (Norm));
               end if;

            else
               if File.Normalized /= null
                 and then FS_String (Norm) & GNAT.OS_Lib.Directory_Separator =
                    File.Normalized.all
               then
                  File.Normalized_And_Resolved := File.Normalized;
               else
                  File.Normalized_And_Resolved := new FS_String'
                    (FS_String (Norm) & GNAT.OS_Lib.Directory_Separator);
               end if;
            end if;
         end;
      end if;
   end Resolve_Symlinks;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (File : not null access Native_File_Record) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Regular_File (String (File.Full.all));
   end Is_Regular_File;

   ----------
   -- Size --
   ----------

   function Size
     (File : not null access Native_File_Record) return Long_Integer
   is
      Fd : constant GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Open_Read
        (String (File.Full.all), Fmode => GNAT.OS_Lib.Binary);
      pragma Warnings (Off);
      Result : constant Long_Integer :=
        Long_Integer (GNAT.OS_Lib.File_Length (Fd));
      pragma Warnings (On);
   begin
      GNAT.OS_Lib.Close (Fd);
      return Result;
   end Size;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (File : not null access Native_File_Record) return Boolean is
   begin
      if GNAT.OS_Lib.Directory_Separator = '\'
        and then File.Full (File.Full'First .. File.Full'First + 1) = "\\"
      then
         --  There is an issue with (at least) GNAT 6.2 when Is_Directory
         --  returns False for Windows network paths (e.g. \\host\shared\).
         --  In this case, we try to open the directory and see if it works.

         declare
            Dir : GNAT.Directory_Operations.Dir_Type;
         begin
            GNAT.Directory_Operations.Open (Dir, String (File.Full.all));
            GNAT.Directory_Operations.Close (Dir);

            return True;

         exception
            when GNAT.Directory_Operations.Directory_Error =>
               return False;
         end;

      else
         return GNAT.OS_Lib.Is_Directory (String (File.Full.all));
      end if;
   end Is_Directory;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (File : not null access Native_File_Record) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Symbolic_Link (String (File.Full.all));
   end Is_Symbolic_Link;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   TZ : constant Time_Offset := UTC_Time_Offset;
   --  Time zone cache, assuming that the OS will not change time zones while
   --  this partition is running.

   function File_Time_Stamp
     (File : not null access Native_File_Record) return Ada.Calendar.Time
   is
      T      : constant GNAT.OS_Lib.OS_Time :=
        GNAT.OS_Lib.File_Time_Stamp (String (File.Full.all));
      Year   : GNAT.OS_Lib.Year_Type;
      Month  : GNAT.OS_Lib.Month_Type;
      Day    : GNAT.OS_Lib.Day_Type;
      Hour   : GNAT.OS_Lib.Hour_Type;
      Minute : GNAT.OS_Lib.Minute_Type;
      Second : GNAT.OS_Lib.Second_Type;

      use type GNAT.OS_Lib.OS_Time;
   begin
      if T = GNAT.OS_Lib.Invalid_Time then
         return GNATCOLL.Utils.No_Time;
      end if;

      GNAT.OS_Lib.GM_Split (T, Year, Month, Day, Hour, Minute, Second);

      return Ada.Calendar.Formatting.Time_Of
        (Year       => Year_Number (Year),
         Month      => Month_Number (Month),
         Day        => Day_Number (Day),
         Hour       => Formatting.Hour_Number (Hour),
         Minute     => Formatting.Minute_Number (Minute),
         Second     => Formatting.Second_Number (Second),
         Sub_Second => 0.0,
         Time_Zone  => TZ);
   end File_Time_Stamp;

   -----------------
   -- Is_Readable --
   -----------------

   function Is_Readable
     (File : not null access Native_File_Record) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Readable_File (String (File.Full.all));
   end Is_Readable;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (File : not null access Native_File_Record) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Writable_File (String (File.Full.all));
   end Is_Writable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (File  : not null access Native_File_Record;
      State : Boolean)
   is
   begin
      if State then
         GNAT.OS_Lib.Set_Writable
           (String (File.Full.all));
      else
         GNAT.OS_Lib.Set_Non_Writable
           (String (File.Full.all));
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (File  : not null access Native_File_Record;
      State : Boolean)
   is
   begin
      if State then
         GNAT.OS_Lib.Set_Readable
           (String (File.Full.all));
      else
         GNAT.OS_Lib.Set_Non_Readable
           (String (File.Full.all));
      end if;
   end Set_Readable;

   ------------
   -- Rename --
   ------------

   procedure Rename
     (From    : not null access Native_File_Record;
      Dest    : not null access Native_File_Record;
      Success : out Boolean)
   is
   begin
      GNAT.OS_Lib.Rename_File
        (String (From.Full.all), String (Dest.Full.all), Success);

   exception
      when others =>
         Success := False;
   end Rename;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From    : not null access Native_File_Record;
      Dest    : FS_String;
      Success : out Boolean)
   is
   begin
      GNAT.OS_Lib.Copy_File
        (String (From.Full.all), String (Dest), Success,
         Mode => GNAT.OS_Lib.Overwrite, Preserve => GNAT.OS_Lib.Full);

   exception
      when others =>
         Success := False;
   end Copy;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (File    : not null access Native_File_Record;
      Success : out Boolean)
   is
   begin
      GNAT.OS_Lib.Delete_File (String (File.Full.all), Success);
   end Delete;

   ---------------------
   -- Read_Whole_File --
   ---------------------

   function Read_Whole_File
     (File : not null access Native_File_Record)
      return GNAT.Strings.String_Access
   is
   begin
      return GNATCOLL.Mmap.Read_Whole_File (String (File.Full.all));

   exception
      when others =>
         return null;
   end Read_Whole_File;

   ---------------------
   -- Read_Whole_File --
   ---------------------

   function Read_Whole_File
     (File : not null access Native_File_Record)
      return GNATCOLL.Strings.XString
   is
   begin
      return GNATCOLL.Mmap.Read_Whole_File (String (File.Full.all));

   exception
      when others =>
         return GNATCOLL.Strings.Null_XString;
   end Read_Whole_File;

   ----------------
   -- Open_Write --
   ----------------

   procedure Open_Write
     (File   : not null access Native_File_Record;
      Append : Boolean := False;
      FD     : out GNAT.OS_Lib.File_Descriptor)
   is
   begin
      if Append then
         FD := GNAT.OS_Lib.Open_Read_Write
           (String (File.Full.all),
            Fmode => GNAT.OS_Lib.Binary);
         GNAT.OS_Lib.Lseek
           (FD, 0, GNAT.OS_Lib.Seek_End);
      else
         FD := GNAT.OS_Lib.Create_File
           (String (File.Full.all),
            Fmode => GNAT.OS_Lib.Binary);
      end if;

   exception
      when others =>
         FD := GNAT.OS_Lib.Invalid_FD;
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close
     (File    : not null access Native_File_Record;
      FD      : GNAT.OS_Lib.File_Descriptor;
      Success : out Boolean)
   is
      pragma Unreferenced (File);
      use type GNAT.OS_Lib.File_Descriptor;
   begin
      if FD /= GNAT.OS_Lib.Invalid_FD then
         GNAT.OS_Lib.Close (FD);
         Success := True;
      else
         Success := False;
      end if;
   end Close;

   ----------------
   -- Change_Dir --
   ----------------

   function Change_Dir
     (Dir : not null access Native_File_Record)
      return Boolean
   is
   begin
      GNAT.Directory_Operations.Change_Dir (String (Dir.Full.all));
      return True;

   exception
      when others =>
         return False;
   end Change_Dir;

   --------------
   -- Read_Dir --
   --------------

   function Read_Dir
     (Dir        : not null access Native_File_Record;
      Dirs_Only  : Boolean := False;
      Files_Only : Boolean := False) return GNAT.Strings.String_List
   is
      Name : constant String :=
               String (Ensure_Directory (Local_FS, Dir.Full.all));
      D    : GNAT.Directory_Operations.Dir_Type;
      Item : String (1 .. 1024);
      Last : Natural;
      Ret  : GNAT.Strings.String_List_Access;
      Tmp  : GNAT.Strings.String_List_Access;
      N    : Natural := 0;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);

   begin
      GNAT.Directory_Operations.Open (D, String (Dir.Full.all));

      loop
         GNAT.Directory_Operations.Read (D, Item, Last);
         exit when Last = 0;

         if Item (1 .. Last) /= "."
           and then Item (1 .. Last) /= ".."
           and then
             (not Dirs_Only
              or else
                GNAT.OS_Lib.Is_Directory (Name & Item (1 .. Last)))
           and then
             (not Files_Only
              or else
                GNAT.OS_Lib.Is_Regular_File (Name & Item (1 .. Last)))
         then
            if Ret = null then
               Ret := new GNAT.Strings.String_List (1 .. 10);

            elsif N = Ret'Last then
               Tmp := new GNAT.Strings.String_List (1 .. Ret'Length * 2);
               Tmp (Ret'Range) := Ret.all;
               Unchecked_Free (Ret);
               Ret := Tmp;
            end if;

            N := N + 1;
            Ret (N) := new String'(Item (1 .. Last));
         end if;
      end loop;

      GNAT.Directory_Operations.Close (D);

      if N = 0 then
         return (1 .. 0 => <>);
      end if;

      declare
         List : constant GNAT.Strings.String_List := Ret (1 .. N);
      begin
         Unchecked_Free (Ret);
         return List;
      end;
   end Read_Dir;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (Dir       : not null access Native_File_Record;
      Recursive : Boolean) return Boolean is
   begin
      if Recursive then
         Ada.Directories.Create_Path (String (Dir.Full.all));
      else
         Ada.Directories.Create_Directory (String (Dir.Full.all));
      end if;

      return True;

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         return False;
   end Make_Dir;

   ----------------
   -- Remove_Dir --
   ----------------

   procedure Remove_Dir
     (Dir       : not null access Native_File_Record;
      Recursive : Boolean;
      Success   : out Boolean)
   is
   begin
      GNAT.Directory_Operations.Remove_Dir (String (Dir.Full.all), Recursive);
      Success := True;

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         Success := False;
   end Remove_Dir;

   --------------
   -- Copy_Dir --
   --------------

   procedure Copy_Dir
     (From    : not null access Native_File_Record;
      Dest    : FS_String;
      Success : out Boolean)
   is
   begin
      if not Is_Directory (From) then
         Success := False;
         return;
      end if;

      if not GNAT.OS_Lib.Is_Directory (String (Dest)) then
         begin
            GNAT.Directory_Operations.Make_Dir (String (Dest));
         exception
            when others =>
               Success := False;
               return;
         end;
      end if;

      declare
         Files : GNAT.Strings.String_List := Read_Dir (From);

      begin
         Success := True;

         for F in Files'Range loop
            declare
               Tmp_From : File_Access :=
                            Create
                              (GNATCOLL.Path.Path
                                 (Local_FS,
                                  "",
                                  From.Full.all,
                                  FS_String (Files (F).all)));
               To_Str   : constant FS_String :=
                            GNATCOLL.Path.Path
                              (Local_FS,
                               "",
                               Dest,
                               FS_String (Files (F).all));
            begin

               if Is_Directory (Tmp_From) then
                  Copy_Dir (Tmp_From, To_Str, Success);
                  Unref (Tmp_From);

                  exit when not Success;

               else
                  Copy (Tmp_From, To_Str, Success);
                  Unref (Tmp_From);

                  exit when not Success;

               end if;
            end;

            GNAT.Strings.Free (Files (F));
         end loop;

      exception
         when others =>
            for J in Files'Range loop
               GNAT.Strings.Free (Files (J));
            end loop;

            Success := False;
      end;

   exception
      when others =>
         Success := False;
   end Copy_Dir;

   ---------------------------
   -- Copy_File_Permissions --
   ---------------------------

   overriding procedure Copy_File_Permissions
     (From, To : not null access Native_File_Record;
      Success  : out Boolean) is
   begin
      GNAT.OS_Lib.Copy_File_Attributes
          (From             => String (From.Full.all),
           To               => String (To.Full.all),
           Success          => Success,
           Copy_Timestamp   => False,
           Copy_Permissions => True);
   end Copy_File_Permissions;

end GNATCOLL.IO.Native;
