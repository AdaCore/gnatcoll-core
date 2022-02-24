------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package body GNATCOLL.File_Paths is

   procedure Append_CWD (Self : in out Any_Path);
   --  Append the current working directory to ``Self``

   function File_Exists (Filename : String) return Boolean;
   --  Return whether the ``Filename`` file exists and is not a Directory

   ----------------
   -- Append_CWD --
   ----------------

   procedure Append_CWD (Self : in out Any_Path) is
   begin
      Self.Directories.Append (To_XString (Current_Directory));
   end Append_CWD;

   -----------------
   -- File_Exists --
   -----------------

   function File_Exists (Filename : String) return Boolean is
   begin
      return Kind (Filename) /= Directory;
   exception
      when Name_Error =>
         return False;
   end File_Exists;

   -----------------
   -- Create_Path --
   -----------------

   function Create_Path
     (Directories : XString_Array;
      CWD         : CWD_Mode := CWD_First) return Any_Path is
   begin
      return Result : Any_Path do
         if CWD = CWD_Last then
            Append_CWD (Result);
         end if;

         for D of reverse Directories loop
            if D.Is_Empty then

               --  Empty components are interpreted as the current directory

               Append_CWD (Result);

            else
               --  Get the absolute file name for ``D``

               declare
                  Abs_Name : constant String :=
                    +Create (+To_String (D), Normalize => True).Full_Name;
               begin
                  Result.Directories.Append (To_XString (Abs_Name));
               end;
            end if;
         end loop;

         if CWD = CWD_First or else Result.Directories.Is_Empty then
            Append_CWD (Result);
         end if;
      end return;
   end Create_Path;

   ------------------------------
   -- Create_Path_From_Environ --
   ------------------------------

   function Create_Path_From_Environ
     (Var_Name  : String;
      Separator : Character := Path_Separator;
      CWD       : CWD_Mode := CWD_First) return Any_Path is
   begin
      return Parse_Path
        (Ada.Environment_Variables.Value (Var_Name, ""),
         Separator,
         CWD);
   end Create_Path_From_Environ;

   ----------------
   -- Parse_Path --
   ----------------

   function Parse_Path
     (Path      : String;
      Separator : Character := Path_Separator;
      CWD       : CWD_Mode := CWD_First) return Any_Path is
   begin
      return Create_Path (To_XString (Path).Split (Separator), CWD);
   end Parse_Path;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory (Path : in out Any_Path; Directory : String) is
   begin
      Path.Directories.Append (To_XString (Directory));
   end Add_Directory;

   ------------
   -- Lookup --
   ------------

   function Lookup (Path : Any_Path; Filename : String) return String is
   begin
      if Create (+Filename).Is_Absolute_Path then

         --  Lookup paths cannot help to find the file if the requested
         --  filename is already absolute: just check if it exists.

         return (if File_Exists (Filename)
                 then Filename
                 else "");
      else
         --  Look for the given file in all directories in ``Path``

         for D of reverse Path.Directories loop
            declare
               F : constant String :=
                 To_String (D) & GNAT.OS_Lib.Directory_Separator & Filename;
            begin
               if File_Exists (F) then
                  return F;
               end if;
            end;
         end loop;

         return "";
      end if;
   end Lookup;

end GNATCOLL.File_Paths;
