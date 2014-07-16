------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with GNATCOLL.Projects;

package GNATCOLL.Scripts.Projects is

   type Project_Tree_Retriever is abstract tagged null record;
   --  This type provides access to project tree for scripting API

   not overriding function Get_Project_Tree
     (Self : Project_Tree_Retriever)
      return GNATCOLL.Projects.Project_Tree_Access is abstract;
   --  Get current project tree

   function Project_Tree return GNATCOLL.Projects.Project_Tree_Access;
   --  Get project tree from assigned project tree retriver pointer

   procedure Register_Commands
     (Repo  : not null access Scripts_Repository_Record'Class;
      Value : not null access Project_Tree_Retriever'Class);
   --  Add script commands for Project class and an object to access project
   --  tree from scripting API.

   --  Next subprogram could be useful to define new commands

   function Get_Project_Class
     (Repo : access Scripts_Repository_Record'Class)
      return Class_Type;
   --  Return the class to use for projects. This encapsulates a Project_Type

   function Get_Data (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.Projects.Project_Type;
   --  Retrieve some project information in Instance

   function Create_Project
     (Script  : access Scripting_Language_Record'Class;
      Project : GNATCOLL.Projects.Project_Type) return Class_Instance;
   --  Return a new project

end GNATCOLL.Scripts.Projects;
