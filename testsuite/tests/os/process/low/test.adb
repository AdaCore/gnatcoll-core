with GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;
with Test_Assert;
with Test_Python;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;

begin
   IO.Put_Line ("GNATCOLL.OS.Process test");

   declare
      Args : OS.Process_Types.Arguments;
      Env  : OS.Process_Types.Environ;
      Status : Integer;
      Output : Unbounded_String;
   begin
      OS.Process_Types.Inherit (Env);

      OS.Process_Types.Add_Argument (Args, Test_Python.Python_Executable);
      OS.Process_Types.Add_Argument (Args, "--version");
      Status := OS.Process.Run (Args, Env);
      A.Assert (Status, 0, Msg => "python --version should return 0");
      OS.Process_Types.Deallocate (Args);

      OS.Process_Types.Add_Argument (Args, Test_Python.Python_Executable);
      OS.Process_Types.Add_Argument (Args, "-c");
      OS.Process_Types.Add_Argument (Args, "print('hello')");

      Output := OS.Process.Run (Args, Env, Status => Status, Strip => True);
      A.Assert (Status, 0,
                Msg => "python -c ""print('hello')"" should return 0");
      A.Assert (To_String (Output), "hello",
                Msg => "python -c ""print('hello')"" output redirection");
      OS.Process_Types.Deallocate (Args);

      IO.Put_Line (
         "Ensure that child process directory correspond to " & "Cwd value");
      OS.Process_Types.Add_Argument (Args, Test_Python.Python_Executable);
      OS.Process_Types.Add_Argument (Args, "-c");
      OS.Process_Types.Add_Argument
         (Args, "import os; import sys; print(os.listdir('.'))");
      Output := OS.Process.Run
         (Args,
          Env,
          Status => Status,
          Cwd    => "process_dir",
          Strip  => True);
      A.Assert (Status, 0);
      A.Assert (To_String (Output), "['hello.txt']");
      OS.Process_Types.Deallocate (Args);

      OS.Process_Types.Import (Env);
      OS.Process_Types.Add_Argument (Args, Test_Python.Python_Executable);
      OS.Process_Types.Add_Argument (Args, "--version");
      Status := OS.Process.Run (Args, Env => Env);
      A.Assert (Status, 0, Msg => "python --version should return 0");
      OS.Process_Types.Deallocate (Args);

   end;
   return A.Report;
end Test;
