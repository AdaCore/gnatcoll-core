------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                        Copyright (C) 2013-2015, AdaCore                  --
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

--  This package provides additional services when using Project Files.
--  These services are of two kinds:
--     - functions to get data types from the underlying implementation
--       of the Project Manager.
--     - subprograms that are usually only used by specialized tools.

pragma Warnings (Off);
pragma Warnings (Off, "*license of withed unit*");
with Prj; use Prj;
pragma Warnings (On, "*license of withed unit*");
pragma Warnings (On);

package GNATCOLL.Projects.Aux is

   function To_Project_Id
     (Project : Projects.Project_Type)
      return Prj.Project_Id;
   pragma Inline (To_Project_Id);
   --  Give access to the Project_Id of a project

   function Project_Tree_Ref_Of
     (Project : Projects.Project_Type)
      return Prj.Project_Tree_Ref;
   pragma Inline (Project_Tree_Ref_Of);
   --  Give access to the Project_Tree_Ref of a project

   function Create_Ada_Mapping_File
     (Project : Projects.Project_Type)
      return String;
   --  Creates a temporary file that contains the mapping of the Ada units
   --  in the project tree rooted at the project Project and returns the full
   --  path of the temporary file. If the creation of this mapping file is
   --  unsuccessful, either an exception is raised or the empty string is
   --  returned.
   --
   --  It is the responsibility of the user to delete this temporary file when
   --  it is no longer needed, either directly or by calling
   --  Delete_All_Temp_Files.

   function Create_Config_Pragmas_File
     (Project : Projects.Project_Type)
      return String;
   --  Creates a temporary file that contains the configuration pragmas for
   --  the project tree rooted at Project and returns the full path of the
   --  temporary file. If the creation of this mapping file is unsuccessful,
   --  either an exception is raised or the empty string is returned.
   --
   --  It is the responsibility of the user to delete this temporary file when
   --  it is no longer needed, either directly or by calling
   --  Delete_All_Temp_Files.

   procedure Delete_All_Temp_Files (Root_Project : Projects.Project_Type);
   --  Delete all the temporary files that have been created by the Project
   --  Manager in the project tree rooted at Root_Project.

end GNATCOLL.Projects.Aux;
