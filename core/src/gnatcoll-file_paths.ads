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

--  Implementation of path-based file lookups (like looking for a program in
--  the ``PATH`` environment variable).

private with Ada.Containers.Vectors;

private with GNAT.OS_Lib;

with GNATCOLL.Strings; use GNATCOLL.Strings;

package GNATCOLL.File_Paths is

   Path_Separator : constant Character;
   --  Default path separator on the current platform

   type Any_Path is private;
   --  Path where to look for files, i.e. sequence of directories

   type CWD_Mode is (If_Empty, CWD_First, CWD_Last);
   --  Control how to include the current working directory (CWD) to a path.
   --
   --  ``If_Empty``: automatically append it if the path would otherwise be
   --  empty.
   --
   --  ``CWD_First``: automatically append it first in the path (i.e. files are
   --  searched in priority in the CWD).
   --
   --  ``CWD_Last``: automatically append it last in the path (i.e. files are
   --  searched in the CWD as a last tentative).

   function Create_Path
     (Directories : XString_Array;
      CWD         : CWD_Mode := CWD_First) return Any_Path;
   --  Create a path for the given ``Directories`` (first directories are
   --  looked up before the next ones). ``CWD`` controls how to include the
   --  current working directory.

   function Create_Path_From_Environ
     (Var_Name  : String;
      Separator : Character := Path_Separator;
      CWD       : CWD_Mode := CWD_First) return Any_Path;
   --  Create a path for the directories listed in the ``Var_Name`` environment
   --  variable. Each path component is separated by ``Separator``. ``CWD``
   --  controls how to include the current working directory.

   function Parse_Path
     (Path      : String;
      Separator : Character := Path_Separator;
      CWD       : CWD_Mode := CWD_First) return Any_Path;
   --  Parse a path from the ``Path`` string. Each path component is separated
   --  by ``Separator``. ``CWD`` controls how to include the current working
   --  directory.

   procedure Add_Directory (Path : in out Any_Path; Directory : String);
   --  Add ``Directory`` to the given ``Path``. This new directory takes
   --  precedence over the existing ones for file lookups.

   function Lookup (Path : Any_Path; Filename : String) return String;
   --  Look for a filed called ``Filename`` in directories referenced by
   --  ``Path`` and return its absolute file name. If the file is not found,
   --  return an empty string.

   Empty_Path : constant Any_Path;
   --  Path for which only lookups on existing absolute file names will succeed

private

   Path_Separator : constant Character := GNAT.OS_Lib.Path_Separator;

   package String_Vectors is new Ada.Containers.Vectors (Positive, XString);

   type Any_Path is record
      Directories : String_Vectors.Vector;
      --  Last directories have precedence over the first ones for file lookups
   end record;

   Empty_Path : constant Any_Path :=
     (Directories => String_Vectors.Empty_Vector);

end GNATCOLL.File_Paths;
