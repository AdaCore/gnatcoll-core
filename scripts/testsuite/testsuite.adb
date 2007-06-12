-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Scripts;          use GNAT.Scripts;
with GNAT.Scripts.Python;   use GNAT.Scripts.Python;
with GNAT.Scripts.Shell;    use GNAT.Scripts.Shell;
with Testsuite_Export;      use Testsuite_Export;
with TestConsole;           use TestConsole;

procedure Testsuite is
   Repo    : Scripts_Repository := new Scripts_Repository_Record;
   Errors  : Boolean;
   Console : aliased Test_Console;
   Lang    : constant String := Argument (1);
   File    : constant String := Argument (2);
begin
   --  Register a single language, so that we can more easily detect
   --  memory leaks in each case

   if Lang = Shell_Name then
      Register_Shell_Scripting (Repo);
   elsif Lang = Python_Name then
      Register_Python_Scripting (Repo, "M");
   end if;
   Register_Standard_Classes (Repo, "Console");

   Testsuite_Export.Register_Functions (Repo);

   Set_Default_Console
     (Lookup_Scripting_Language (Repo, Lang), Console'Unchecked_Access);

   Execute_File
     (Script       => Lookup_Scripting_Language (Repo, Lang),
      Filename     => File,
      Show_Command => False,
      Errors       => Errors);
   if Errors then
      Put_Line ("Errors were reported by Execute_File");
   end if;

   Free (Console);
   Destroy (Repo);

end Testsuite;
