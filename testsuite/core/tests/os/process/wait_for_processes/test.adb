with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;

   H1, H2, H3, Result1, Result2, Result3 : Process_Handle;
   Args   : Argument_List;
   Status : Integer;
begin
   IO.Put_Line ("GNATCOLL.OS.Process.Wait_For_Processes");
   IO.Put_Line ("First test the function with two python processes");
   Args.Append (Test_Python.Python_Executable);
   Args.Append ("./sleep5.py");

   H1 := Start (Args);
   H2 := Start (Args);

   Result1 := Wait_For_Processes (Processes => (H1, H2), Timeout => 2.0);
   A.Assert (Result1 = Invalid_Handle,
             "no process should be in waiting state");

   Result1 := Wait_For_Processes (Processes => (H1, H2), Timeout => 5.0);
   A.Assert (Result1 /= Invalid_Handle, "a process should be available");
   if Result1 /= Invalid_Handle then
      Status := Wait (Result1);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   Result2 := Wait_For_Processes (Processes => (H1, H2), Timeout => 5.0);
   A.Assert (Result2 /= Invalid_Handle, "a process should be available");
   A.Assert (Result1 /= Result2, "Resutl1 should be different from Result2");
   if Result2 /= Invalid_Handle then
      Status := Wait (Result2);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   Result3 := Wait_For_Processes (Processes => (H1, H2), Timeout => 0.0);
   A.Assert (Result3 = Invalid_Handle,
             "no process should be in waitable state");

   --  This test is important specially on Unix where sigchld is received for
   --  all processes even when not monitored
   IO.Put_Line ("Do some tests waiting on distinct processes");
   H3 := Start (Args);
   delay 0.5;
   H1 := Start (Args);
   H2 := Start (Args);

   Result3 := Wait_For_Processes (Processes => (H1, H2), Timeout => 6.0);
   A.Assert (Result3 = H1 or else Result3 = H2, "H1 or H2 expected");
   if Result3 = H1 or else Result3 = H2 then
      Status := Wait (Result3);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   Result3 := Wait_For_Processes (Processes => (H1, H2), Timeout => 6.0);
   A.Assert (Result3 = H1 or else Result3 = H2, "H1 or H2 expected");
   if Result3 = H1 or else Result3 = H2 then
      Status := Wait (Result3);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   Result3 := Wait_For_Processes (Processes => (1 => H3), Timeout => 1.0);
   A.Assert (Result3 = H3, "H3 expected");
   if Result3 = H3 then
      Status := Wait (Result3);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   return A.Report;
end Test;
