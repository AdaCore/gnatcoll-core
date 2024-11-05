with GNATCOLL.OS.Process;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;

begin
   IO.Put_Line ("GNATCOLL.OS.Process stderr test");

   declare
      Args : OS.Process.Argument_List;
      Status : Integer;
   begin
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("-c");
      Args.Append ("import sys; sys.stderr.write('one error')");

      Status := OS.Process.Run (Args);
      A.Assert
         (Status, 0, "a status different from 0 means stderr write failed");
   end;
   return A.Report;
end Test;
