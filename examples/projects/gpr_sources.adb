------------------------------------------------------------------------------
--                                                                          --
--                     G N A T C O L L   E X A M P L E S                    --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it and/or modify it         --
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

--  This example lists all sources belonging to a project.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

procedure GPR_Sources is

   package Args is

      Parser : Argument_Parser := Create_Argument_Parser
        (Help =>
            "Lists all sources belonging to a project.");

      package Project_File is new Parse_Option
        (Parser      => Parser,
         Short       => "-P",
         Long        => "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Scenario_Vars is new Parse_Option_List
        (Parser     => Parser,
         Short      => "-X",
         Long       => "--variables",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Help       => "Scenario variables to pass to the project file");

      package Target is new Parse_Option
        (Parser       => Parser,
         Short       => "-t",
         Long         => "--target",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Custom target to use");

      package Runtime is new Parse_Option
        (Parser       => Parser,
         Long         => "--RTS",
         Arg_Type     => Unbounded_String,
         Default_Val  => Null_Unbounded_String,
         Help         => "Custom runtime to use");

      package Recursive is new Parse_Flag
        (Parser => Parser,
         Short  => "-r",
         Long   => "--recursive",
         Help   => "Include sources from all dependencies");

   end Args;

   Env   : Project_Environment_Access;
   Tree  : Project_Tree;
   Files : File_Array_Access;

   function Init_Project return Boolean;
   --  Initializes the project tree using command line options supplied by
   --  Args. Returns True on success, or False otherwise.

   function Init_Project return Boolean is
   begin
      declare
         Filename : constant String := To_String (Args.Project_File.Get);
         Target   : constant String := To_String (Args.Target.Get);
         Runtime  : constant String := To_String (Args.Runtime.Get);
      begin
         if Filename = "" then
            Put_Line ("Project file not specified.");
            return False;
         end if;

         Initialize (Env);
         Env.Set_Target_And_Runtime (Target, Runtime);

         for Assoc of Args.Scenario_Vars.Get loop
            declare
               A        : constant String := To_String (Assoc);
               Eq_Index : Natural := A'First;
            begin
               while Eq_Index <= A'Length and then A (Eq_Index) /= '=' loop
                  Eq_Index := Eq_Index + 1;
               end loop;
               if Eq_Index not in A'Range then
                  Put_Line ("Invalid scenario variable: -X" & A);
                  return False;
               end if;
               Change_Environment
                 (Env.all,
                  A (A'First .. Eq_Index - 1),
                  A (Eq_Index + 1 .. A'Last));
            end;
         end loop;

         Tree.Load (GNATCOLL.VFS.Create (+Filename), Env);

         if Predefined_Source_Files (Env)'Length = 0 then
            Put_Line ("Toolchain not found for this target/runtime.");
            return False;
         end if;

         return True;
      end;
   end Init_Project;

begin

   if not Args.Parser.Parse then
      return;
   end if;

   if not Init_Project then
      Put_Line ("Could not initialize project.");
      return;
   end if;

   --  List the source files for project

   Files := Tree.Root_Project.Source_Files
     (Recursive                => Args.Recursive.Get,
      Include_Externally_Built => False);

   for F in Files'Range loop
      Put_Line (Files (F).Display_Full_Name);
   end loop;

   Unchecked_Free (Files);
   Tree.Unload;
   Free (Env);
end GPR_Sources;
