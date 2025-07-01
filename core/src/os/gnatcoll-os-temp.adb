--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Random;
with GNATCOLL.OS.Constants;
with GNATCOLL.OS.Stat;
with GNATCOLL.OS.FSUtil;
with GNAT.OS_Lib;
with Interfaces;

package body GNATCOLL.OS.Temp is

   Startup_System_Temp_Dir : Path_Access := null;

   procedure Free is
      new Ada.Unchecked_Deallocation (UTF8.UTF_8_String, Path_Access);

   MERSENNE_RESEED_PERIOD : constant Integer := 623;
   --  In order not to be able to guess the next temp file name we need to
   --  ensure that we reseed with a safe random generator before reaching 624
   --  iterations. After 624 iterations with the same seed the seed can be
   --  guess and thus possibly the next temp file leaving room for DoS attacks.

   package Ada_Rand is new Ada.Numerics.Discrete_Random
      (Interfaces.Unsigned_64);
   package Stat renames GNATCOLL.OS.Stat;
   --  Instantiate the Mersenne twister provided by the Ada Runtime

   --  It seems that indirection is needed as pragma Thread_Local_Storage does
   --  not accept diretory the Generator type.
   type Generator_Access is access Ada_Rand.Generator;

   --  In order to be thread-safe use thread local storage to keep the PRNG
   --  state.
   Mersenne_Generator : Generator_Access := null;
   pragma Thread_Local_Storage (Mersenne_Generator);

   --  The initial value ensure that the generator is seeded with a secure
   --  random source on first use.
   Mersenne_Iterations : Integer := MERSENNE_RESEED_PERIOD;
   pragma Thread_Local_Storage (Mersenne_Iterations);

   --  function Random_Part return UTF8.UTF_8_String;
   --  Function that returns the random part of a temp file or directory.
   --  The implementation use a Mersenne Twister seeded on a regular basis
   --  by the system secure PRNG. Having a secure name generation prevents
   --  some DoS attack.

   function Base32_Image (I : Interfaces.Unsigned_64) return String;
   --  Use base32 to encode the random parts of filename. Base32 is safe on
   --  all filesystems even the one that are not case sensitive.

   procedure Init;
   --  Initialize location of system default temporary dir

   procedure Init is separate;

   procedure Check_No_Dir_Sep (Str : UTF8.UTF_8_String);
   --  Check that Str does not contain a dir separator. If a dir separator is
   --  present raise OS_Error. This checks is used to ensure that Prefix and
   --  Suffix passed to the function of this package cannot contain hidden
   --  directories.

   function Generate_Random_Path
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "")
      return UTF8.UTF_8_String;
   --  Generate a random path. The function does not check whether the path
   --  exists or not.

   ------------------
   -- Base32_Image --
   ------------------

   Base32_Table : constant array (0 .. 31) of Character :=
      ('0', '1', '2', '3', '4', '5', '6', '7',
       '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
       'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
       'q', 'r', 's', 't', 'u', 'v', 'w', 'x');

   function Base32_Image (I : Interfaces.Unsigned_64) return String
   is
      use type Interfaces.Unsigned_64;
      Result : String (1 .. 13);
      Tmp    : Interfaces.Unsigned_64 := I;
   begin
      for I in reverse 1 .. 13 loop
         Result (I) := Base32_Table (Integer (Tmp mod 32));
         Tmp := Tmp / 32;
      end loop;
      return Result;
   end Base32_Image;

   ----------------------
   -- Check_No_Dir_Sep --
   ----------------------

   procedure Check_No_Dir_Sep (Str : UTF8.UTF_8_String) is
   begin
      for C of Str loop
         if C = '/' or else C = GNATCOLL.OS.Constants.Dir_Sep then
            raise OS_Error with
               "prefix of random path cannot contain a dir separator";
         end if;
      end loop;
   end Check_No_Dir_Sep;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Temp_Dir_Handle) is
   begin
      if Self.Path /= null then
         if Self.Auto_Delete then
            if not GNATCOLL.OS.FSUtil.Remove_Tree (Self.Path.all) then
               pragma Annotate
                  (Xcov, Exempt_On,
                   "Hard to simulate a rm failure on all systems");
               raise OS_Error with "cannot delete temporary directory";
               pragma Annotate (Xcov, Exempt_Off);
            end if;
         end if;
         Free (Self.Path);
      end if;
   end Finalize;

   procedure Finalize (Self : in out Temp_File_Handle) is

   begin
      if Self.Path /= null then
         FS.Close (Self.FD);
         if Self.Auto_Delete then
            if not GNATCOLL.OS.FSUtil.Remove_File (Self.Path.all) then
               pragma Annotate
                  (Xcov, Exempt_On,
                   "Hard to simulate a rm failure on all systems");
               raise OS_Error with "cannot delete temporary file";
               pragma Annotate (Xcov, Exempt_Off);
            end if;
         end if;
         Free (Self.Path);
      end if;
   end Finalize;

   -----------------
   -- Random_Part --
   -----------------

   function Random_Part return UTF8.UTF_8_String
   is
   begin
      --  First instantiation of the iterator
      if Mersenne_Generator = null then
         Mersenne_Generator := new Ada_Rand.Generator;
      end if;

      --  Reseed if necessary
      if Mersenne_Iterations >= MERSENNE_RESEED_PERIOD then
         Ada_Rand.Reset
            (Mersenne_Generator.all, GNATCOLL.Random.Random_Integer);
         Mersenne_Iterations := 0;
      end if;

      --  Get the random portion of the filename
      Mersenne_Iterations := Mersenne_Iterations + 1;

      return Base32_Image (Ada_Rand.Random (Mersenne_Generator.all));
   end Random_Part;

   --------------------------
   -- Generate_Random_Path --
   --------------------------

   function Generate_Random_Path
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "")
      return UTF8.UTF_8_String
   is
   begin
      return GNAT.OS_Lib.Normalize_Pathname
         (Dir &
          GNATCOLL.OS.Constants.Dir_Sep &
          Prefix &
          Random_Part &
          Suffix,
          Resolve_Links => False);
   end Generate_Random_Path;

   -----------------
   -- Random_Path --
   -----------------

   function Random_Path
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Max_Attempts  : Integer           := DEFAULT_MAX_ATTEMPTS)
      return UTF8.UTF_8_String
   is
      Attempts : Integer := 0;
   begin
      Check_No_Dir_Sep (Prefix);
      Check_No_Dir_Sep (Suffix);

      loop
         declare
            Result : constant UTF8.UTF_8_String :=
               Generate_Random_Path (Prefix, Suffix, Dir);
         begin
            if not Stat.Exists (Stat.Stat (Result)) then
               return Result;
            end if;

            --  Ensure number of attempts is limited
            pragma Annotate
               (Xcov, Exempt_On,
                "No testing as next random path cannot be predicted");

            Attempts := Attempts + 1;
            if Attempts >= Max_Attempts then
               raise OS_Error with "cannot find a random path";
            end if;

            pragma Annotate (Xcov, Exempt_Off);
         end;
      end loop;
   end Random_Path;

   --------------
   -- Temp_Dir --
   --------------

   function Create_Temp_Dir
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Auto_Delete  : Boolean           := True;
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return Temp_Dir_Handle
   is
      Attempts : Integer := 0;
   begin
      Check_No_Dir_Sep (Prefix);
      Check_No_Dir_Sep (Suffix);

      loop
         declare
            Result_Path : constant UTF8.UTF_8_String :=
               Generate_Random_Path (Prefix, Suffix, Dir);
            Dir_Created : Boolean;
         begin
            Dir_Created := GNATCOLL.OS.FSUtil.Create_Directory (Result_Path);
            if Dir_Created then
               return Result : Temp_Dir_Handle do
                  Result.Path := new UTF8.UTF_8_String'(Result_Path);
                  Result.Auto_Delete := Auto_Delete;
               end return;
            end if;

            --  Ensure number of attempts is limited
            Attempts := Attempts + 1;
            if Attempts >= Max_Attempts then
               raise OS_Error with "cannot create a temp directory";
            end if;
         end;
      end loop;
   end Create_Temp_Dir;

   function Create_Temp_Dir
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return UTF8.UTF_8_String
   is
   begin
      return Create_Temp_Dir
         (Prefix, Suffix, Dir, False, Max_Attempts).Path.all;
   end Create_Temp_Dir;

   ----------
   -- Path --
   ----------

   function Path (Temp_Dir : Temp_Dir_Handle) return UTF8.UTF_8_String is
   begin
      return Temp_Dir.Path.all;
   end Path;

   ----------------------
   -- Create_Temp_File --
   ----------------------

   function Create_Temp_File
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Auto_Delete  : Boolean           := True;
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return Temp_File_Handle
   is
      Attempts : Integer := 0;
      use all type FS.File_Descriptor;
   begin
      Check_No_Dir_Sep (Prefix);
      Check_No_Dir_Sep (Suffix);

      loop
         declare
            Result_Path : constant UTF8.UTF_8_String :=
               Generate_Random_Path (Prefix, Suffix, Dir);
            FD : FS.File_Descriptor;
         begin
            FD := FS.Open (Result_Path, Mode => FS.Create_Mode);
            if FD /= FS.Invalid_FD then
               return Result : Temp_File_Handle do
                  Result.Path := new UTF8.UTF_8_String'(Result_Path);
                  Result.FD := FD;
                  Result.Auto_Delete := Auto_Delete;
               end return;
            end if;

            --  Ensure number of attempts is limited
            Attempts := Attempts + 1;
            if Attempts >= Max_Attempts then
               raise OS_Error with "cannot create a temp file";
            end if;
         end;
      end loop;
   end Create_Temp_File;

   ----------
   -- Path --
   ----------

   function Path (Temp_File : Temp_File_Handle) return UTF8.UTF_8_String
   is
   begin
      return Temp_File.Path.all;
   end Path;

   ---------------------
   -- File_Descriptor --
   ---------------------

   function File_Descriptor
      (Temp_File : Temp_File_Handle) return FS.File_Descriptor
   is
   begin
      return Temp_File.FD;
   end File_Descriptor;

   ---------------------
   -- System_Temp_Dir --
   ---------------------

   function System_Temp_Dir return UTF8.UTF_8_String is
   begin
      if Startup_System_Temp_Dir = null then
         Init;
      end if;
      return Startup_System_Temp_Dir.all;
   end System_Temp_Dir;

   -------------------------
   -- Set_System_Temp_Dir --
   -------------------------

end GNATCOLL.OS.Temp;
