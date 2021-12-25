with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;

   H         : Process_Handle;
   Args      : Argument_List;
   Iteration : Integer := 0;
   Status    : Integer;
begin
   IO.Put_Line ("GNATCOLL.OS.Process State");

   Args.Append (Test_Python.Python_Executable);
   Args.Append ("./sleep2.py");

   H := Start (Args);

   for K in 1 .. 100 loop
      Iteration := Iteration + 1;
      IO.Put_Line (State (H)'Img);
      exit when State (H) /= RUNNING;
      delay 0.1;
   end loop;

   A.Assert (Iteration < 24, "Expect around 20 calls to wait");
   A.Assert
     (Iteration >= 12, "Check that we wait a few times," & Iteration'Img);
   A.Assert (State (H) = WAITABLE, "Check that we are in a waitable state");
   if State (H) = WAITABLE then
      Status := Wait (H);
      A.Assert (Status, 42, "Check that returned status is 42");
      A.Assert (State (H) = TERMINATED);
   end if;
   return A.Report;
end Test;
