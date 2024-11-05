with GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;
with GNATCOLL.OS.FS;
with Test_Assert;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with GNAT.IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;
   package PT renames GNATCOLL.OS.Process_Types;
   package FS renames GNATCOLL.OS.FS;

   Args   : PT.Arguments;
   Env    : PT.Environ;
   Status : Integer;
   Output : Unbounded_String;
begin
   PT.Inherit (Env);

   if Ada.Command_Line.Argument_Count = 0 then
      --  This the topelevel program that check redirections
      IO.Put_Line ("Test redirection to null file");
      PT.Add_Argument (Args, Ada.Command_Line.Command_Name);
      PT.Add_Argument (Args, "to_dev_null");
      Output := OS.Process.Run
         (Args, Env, Status => Status, Stderr => FS.To_Stdout);
      A.Assert (Status, 0, Msg => "program should return 0 as status");
      --  This ensure that redirection of stdout and stderr to /dev/null by the
      --  child process is not leaked on stdout and or stderr
      A.Assert (To_String (Output), "");
      PT.Deallocate (Args);

      IO.Put_Line ("Test redirection of stderr to stdout");
      PT.Add_Argument (Args, Ada.Command_Line.Command_Name);
      PT.Add_Argument (Args, "to_stdout");
      Output := OS.Process.Run
         (Args, Env,
          Status            => Status,
          Stderr            => FS.Null_FD,
          Strip             => True,
          Universal_Newline => True);
      A.Assert (Status, 0, Msg => "program should return 0 as status");
      A.Assert
         (To_String (Output), "hello stdout" & ASCII.LF & "hello stderr");

      PT.Deallocate (Args);

      return A.Report;

   elsif Ada.Command_Line.Argument (1) = "to_dev_null" then
      --  This the version of the program spawned by itself
      PT.Add_Argument (Args, Ada.Command_Line.Command_Name);
      PT.Add_Argument (Args, "write_to_stdout_and_stderr");
      Status := OS.Process.Run
         (Args, Env, Stdout => FS.Null_FD, Stderr => FS.To_Stdout);

   elsif Ada.Command_Line.Argument (1) = "to_stdout" then
      PT.Add_Argument (Args, Ada.Command_Line.Command_Name);
      PT.Add_Argument (Args, "write_to_stdout_and_stderr");
      Status := OS.Process.Run (Args, Env, Stderr => FS.To_Stdout);

   elsif Ada.Command_Line.Argument (1) = "write_to_stdout_and_stderr" then
      IO.Put_Line (IO.Standard_Output, "hello stdout");
      IO.Put_Line (IO.Standard_Error, "hello stderr");
   end if;

   return 0;
end Test;
