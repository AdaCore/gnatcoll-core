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

with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Scripts;          use GNAT.Scripts;
with GNAT.Scripts.Python;   use GNAT.Scripts.Python;
with GNAT.Scripts.Shell;    use GNAT.Scripts.Shell;
with Testsuite_Export;      use Testsuite_Export;
with TestConsole;           use TestConsole;

procedure TestAPI is
   use String_Lists;
   Repo    : Scripts_Repository;
   Console : aliased Test_Console;

   procedure Completions (Input : String; Lang : Scripting_Language);
   procedure Completions (Input : String; Lang : Scripting_Language) is
      Completions : String_Lists.List;
      C : String_Lists.Cursor;
   begin
      Put_Line ("Completions for " & Input & " (" & Get_Name (Lang) & "):");
      Complete (Lang, Input, Completions);
      C := First (Completions);
      while Has_Element (C) loop
         Put (Element (C) & ", ");
         Next (C);
      end loop;
      New_Line;
   end Completions;

   Py : Scripting_Language;
   Sh : Scripting_Language;

begin
   Initialize (Repo);
   Register_Shell_Scripting (Repo);
   Register_Python_Scripting (Repo, "M");
   Register_Standard_Classes (Repo, "Console");

   Testsuite_Export.Register_Functions (Repo);

   Py := Lookup_Scripting_Language (Repo, "python");
   Sh := Lookup_Scripting_Language (Repo, "shell");

   Set_Default_Console (Py, Console'Unchecked_Access);
   Completions ("No", Py);
   Completions ("M", Py);
   Completions ("M.C", Py);
   Completions ("M.C1.", Py);
   Completions ("M.C1.me", Py);

   Set_Default_Console (Sh, Console'Unchecked_Access);
   Completions ("C1", Sh);
   Completions ("", Sh);
   Completions ("Ba", Sh);

   Free (Console);
   Destroy (Repo);
end TestAPI;
