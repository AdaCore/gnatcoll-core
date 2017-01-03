------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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

with GPR.Env;
with GPR.Names;                use GPR.Names;
with GPR.Snames;

package body GNATCOLL.Projects.Aux is

   -------------------
   -- To_Project_Id --
   -------------------

   function To_Project_Id
     (Project : Projects.Project_Type)
      return GPR.Project_Id
   is
   begin
      return Project.Data.View;
   end To_Project_Id;

   -------------------------
   -- Project_Tree_Ref_Of --
   -------------------------

   function Project_Tree_Ref_Of
     (Project : Projects.Project_Type)
      return GPR.Project_Tree_Ref
   is
   begin
      return Tree_View (Project);
   end Project_Tree_Ref_Of;

   -----------------------------
   -- Create_Ada_Mapping_File --
   -----------------------------

   function Create_Ada_Mapping_File
     (Project : Projects.Project_Type)
      return String
   is
      Name : Path_Name_Type;
   begin
      GPR.Env.Create_Mapping_File
        (Project  => Project.Data.View,
         Language => Snames.Name_Ada,
         In_Tree  => Tree_View (Project),
         Name     => Name);

      if Name = No_Path then
         return "";

      else
         return Get_Name_String (Name);
      end if;
   end Create_Ada_Mapping_File;

   --------------------------------
   -- Create_Config_Pragmas_File --
   --------------------------------

   function Create_Config_Pragmas_File
     (Project : Projects.Project_Type)
      return String
   is
   begin
      GPR.Env.Create_Config_Pragmas_File
        (For_Project  => Project.Data.View,
         In_Tree      => Tree_View (Project));

      declare
         Path : constant Path_Name_Type := Project.Data.View.Config_File_Name;
      begin
         if Path = No_Path then
            return "";

         else
            return Get_Name_String (Path);
         end if;
      end;
   end Create_Config_Pragmas_File;

   ---------------------------
   -- Delete_All_Temp_Files --
   ---------------------------

   procedure Delete_All_Temp_Files (Root_Project : Projects.Project_Type) is
   begin
      GPR.Delete_All_Temp_Files (Tree_View (Root_Project).Shared);
   end Delete_All_Temp_Files;

end GNATCOLL.Projects.Aux;
