--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
--  Package to create temporary paths, files and directories.
--

with Ada.Strings.UTF_Encoding;
with Ada.Finalization;
with GNATCOLL.OS.FS;

package GNATCOLL.OS.Temp is

   package UTF8 renames Ada.Strings.UTF_Encoding;
   package FS renames GNATCOLL.OS.FS;

   --  In order not to be subject to temp file name attacks, this package
   --  ensure that generated paths cannot be guessed easily. For that a
   --  Mersenne generator reseeded on a regular basis using the system
   --  generator (which is cryptographically safe) is used. In addition each
   --  task is using a distinct generator.

   type Temp_Dir_Handle is limited private;
   --  Descriptor of a temp directory

   type Temp_File_Handle is limited private;
   --  Descriptor of a temp file

   function Random_Part return UTF8.UTF_8_String;
   --  Generator used by the following functions in order to create the random
   --  part of the resource path.
   --  It might be useful in order to generate random names for other resources
   --  not handled by the present API.

   DEFAULT_MAX_ATTEMPTS : constant Integer := 16 * 1024;
   --  Default number of attempts used by Random_Path, Create_Temp_Dir and
   --  Create_Temp_File functions.

   function Random_Path
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return UTF8.UTF_8_String;
   --  Even if not secure some app requires to just get a valid non existing
   --  path without opening the file. In that case the API cannot set
   --  permissions or ensure that the file is not created in the meantime.
   --  Use of a good random generator mitigate that issue (see former
   --  comments).
   --  The generated path has the following form:
   --
   --      Dir/Prefix[RANDOMPART]Suffix
   --
   --  The random part of the path has always a 13 characters length and use
   --  only lower case characters and numbers (base32 encoding of an 64bits
   --  unsigned integer.
   --  If after Max_Attempts the function cannot generate a valid path,
   --  OS_Error is raised.

   function Create_Temp_Dir
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Auto_Delete  : Boolean           := True;
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return Temp_Dir_Handle;
   --  Create a temporary directory
   --  See Random_Path function documentation for meaning of Prefix, Suffix,
   --  Dir and Max_Attempts parameters.
   --  If Auto_Delete is True then the directory is deleted when the
   --  Temp_Dir_Handle is automatically finalized.

   function Create_Temp_Dir
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return UTF8.UTF_8_String;
   --  Same as previous function except that a path is directly returned. In
   --  that case Auto_Delete is set implicitely to False.

   function Path (Temp_Dir : Temp_Dir_Handle) return UTF8.UTF_8_String;
   --  Get the path the temporary directory.

   function Create_Temp_File
      (Prefix       : UTF8.UTF_8_String := "";
       Suffix       : UTF8.UTF_8_String := "";
       Dir          : UTF8.UTF_8_String := "";
       Auto_Delete  : Boolean           := True;
       Max_Attempts : Integer           := DEFAULT_MAX_ATTEMPTS)
      return Temp_File_Handle;
   --  Create and open a temporary file
   --  See Random_Path function documentation for meaning of Prefix, Suffix,
   --  Dir and Max_Attempts parameters.
   --  If Auto_Delete is True then the file is deleted when the
   --  Temp_File_Handle is automatically finalized.

   function Path (Temp_File : Temp_File_Handle) return UTF8.UTF_8_String;
   --  Retrieve the path associated with a Temp_File_Handle.

   function File_Descriptor
      (Temp_File : Temp_File_Handle) return FS.File_Descriptor;
   --  Retrieve the file descriptor of the file associated a Temp_File_Handle.

   function System_Temp_Dir return UTF8.UTF_8_String;
   --  Return the default system temporary directory.
   --
   --  Note that the returned value is constant. Any change in the environment
   --  after the first call will be ignored.
   --
   --  The following heuristic is followed on non Windows systems:
   --
   --    1- Use TMPDIR environment variable value
   --    2- Use TEMP environment variable value
   --    3- Use TMP environment variable value
   --    4- Use /tmp
   --    5- Use /var/tmp
   --    6- Use /usr/tmp
   --
   --  The function returns the first value that is a valid path to a
   --  directory.
   --
   --  On Windows system the order of lookup is different
   --
   --    1- Use TMPDIR environment variable value
   --    2- Use TEMP environment variable value
   --    3- Use TMP environment variable value
   --    4- Use %USERPROFILE%\AppData\Local\Temp
   --    5- Use %SYSTEMROOT\Temp
   --    6- Use \temp
   --    7- Use \tmp
   --    8- Use C:\temp
   --    9- Use C:\tmp

private

   type Path_Access is access all UTF8.UTF_8_String;

   type Temp_Dir_Handle is new Ada.Finalization.Limited_Controlled with record
      Path        : Path_Access := null;
      Auto_Delete : Boolean := True;
   end record;

   procedure Finalize (Self : in out Temp_Dir_Handle);

   type Temp_File_Handle is new Ada.Finalization.Limited_Controlled with record
      FD          : FS.File_Descriptor := FS.Invalid_FD;
      Path        : Path_Access := null;
      Auto_Delete : Boolean := True;
   end record;

   procedure Finalize (Self : in out Temp_File_Handle);

end GNATCOLL.OS.Temp;
