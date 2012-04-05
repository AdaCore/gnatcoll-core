------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;          use Ada.Calendar;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Command_Line;     use GNAT.Command_Line;
with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.ALI;          use GNATCOLL.ALI;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Postgres;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

procedure Test_Entities is
   Use_Postgres : aliased Boolean := False;
   --  Whether to use sqlite or postgreSQL

   Do_Not_Perform_Queries : aliased Boolean := False;
   --  Whether to perform the queries in the database

   DB_Name     : aliased String_Access;
   Tmp_DB_Name : aliased String_Access;

   Omit_Runtime_Files : aliased Boolean := False;
   --  Whether to parse files from predefined directories

   GPR_File     : Virtual_File;

   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   Absolute_Start : Time;
   GNAT_Version : String_Access;
   Cmdline_Config : Command_Line_Configuration;
   Session : Session_Type;

begin
   GNATCOLL.Traces.Parse_Config_File;

   Define_Switch
     (Cmdline_Config, Do_Not_Perform_Queries'Access,
      Long_Switch => "--nodb",
      Help => "Disable all SQL commands (timing measurement only)");
   Define_Switch
     (Cmdline_Config, Use_Postgres'Access,
      Long_Switch => "--postgres",
      Help => "Use postgreSQL as the backend, instead of sqlite");
   Define_Switch
     (Cmdline_Config, Tmp_DB_Name'Access,
      Long_Switch => "--tmpdb:",
      Help =>
        "Name of the temporary database (use :memory: to copy to memory)");
   Define_Switch
     (Cmdline_Config, DB_Name'Access,
      Long_Switch => "--db:",
      Help => "Name of the database");
   Define_Switch
     (Cmdline_Config, Omit_Runtime_Files'Access,
      Long_Switch => "--noruntime",
      Help => "Do not parse runtime files");

   Getopt (Cmdline_Config);

   if DB_Name.all = "" then
      Free (DB_Name);
      DB_Name := new String'("entities.db");
   end if;

   if Tmp_DB_Name.all = "" then
      Free (Tmp_DB_Name);
      Tmp_DB_Name := new String'(":memory:");
   end if;

   GPR_File := Create (+GNAT.Command_Line.Get_Argument);

   GNATCOLL.SQL.Exec.Perform_Queries := not Do_Not_Perform_Queries;

   --  Prepare database

   if Use_Postgres then
      GNATCOLL.SQL.Sessions.Setup
        (Descr        => GNATCOLL.SQL.Postgres.Setup (Database => DB_Name.all),
         Max_Sessions => 1);
   else
      GNATCOLL.SQL.Sessions.Setup
        (Descr => GNATCOLL.SQL.Sqlite.Setup (Database => Tmp_DB_Name.all),
         Max_Sessions => 1);
   end if;

   --  Load project

   Initialize (Env);
   Env.Set_Path_From_Gnatls
     (Gnatls       => "gnatls",
      GNAT_Version => GNAT_Version,
      Errors       => Put_Line'Access);
   Env.Register_Default_Language_Extension
     (Language_Name       => "C",
      Default_Spec_Suffix => ".h",
      Default_Body_Suffix => ".c");
   Free (GNAT_Version);
   Tree.Load
     (Root_Project_Path => GPR_File,
      Env               => Env,
      Errors            => Put_Line'Access);

   --  Parse LI files (loading and dumping to DB_Name)

   Absolute_Start := Clock;
   Session := Get_New_Session;
   Parse_All_LI_Files
     (DB           => Session.DB,
      Tree         => Tree,
      Project      => Tree.Root_Project,
      Parse_Runtime_Files => not Omit_Runtime_Files,
      From_DB_Name => DB_Name.all,
      To_DB_Name   => DB_Name.all);
   Put_Line (Duration'Image (Clock - Absolute_Start) & " s");

   --  Free memory

   Tree.Unload;
   Free (Env);
   GNATCOLL.Projects.Finalize;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null;
end Test_Entities;
