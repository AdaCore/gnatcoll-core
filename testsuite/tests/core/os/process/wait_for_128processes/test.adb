with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;

   Handles : Process_Array (1 .. 128) := (others => Invalid_Handle);
   Args   : Argument_List;
   Result : Integer;
begin
   IO.Put_Line ("GNATCOLL.OS.Process.Wait_For_Processes");
   IO.Put_Line ("First test the function with two python processes");
   Args.Append (Test_Python.Python_Executable);
   Args.Append ("./sleep.py");
   Args.Append ("16");

   for Index in 1 .. 127 loop
      Handles (Index) := Start (Args);
   end loop;

   Args.Clear;
   Args.Append (Test_Python.Python_Executable);
   Args.Append ("./sleep.py");
   Args.Append ("2");
   Handles (128) := Start (Args);

   for Index in 1 .. 128 loop
      Result := Wait_For_Processes (Processes => Handles, Timeout => 61.0);
      A.Assert (Result >= 0,
                "A waitable process should be available");
      if Result >= 0 then
         A.Assert (Handles (Result) /= Invalid_Handle,
                   "ensure we don't wait twice on the same process");
         A.Assert (Wait (Handles (Result)), 42, "except exit status of 42");
         Handles (Result) := Invalid_Handle;
      end if;
   end loop;

   Result := Wait_For_Processes (Processes => Handles, Timeout => 61.0);
   A.Assert (Result, WAIT_NO_PROCESS, "no more processes to wait");
   return A.Report;
end Test;
