with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   procedure Block_Sigchld;
   pragma Import (C, Block_Sigchld, External_Name => "block_sigchld");

   procedure Restore_Sigmask;
   pragma Import (C, Restore_Sigmask, External_Name => "restore_sigmask");

   package A renames Test_Assert;
   package IO renames GNAT.IO;

   H1, Result1 : Process_Handle;
   Args   : Argument_List;
   Status : Integer;
begin
   Block_Sigchld;

   IO.Put_Line ("GNATCOLL.OS.Process.Wait_For_Processes with SIGCHLD blocked");
   Args.Append (Test_Python.Python_Executable);
   Args.Append ("./sleep5.py");

   H1 := Start (Args);

   Result1 := Wait_For_Processes (Processes => (1 => H1), Timeout => 10.0);
   A.Assert (Result1 /= Invalid_Handle, "a process should be available");
   if Result1 /= Invalid_Handle then
      Status := Wait (Result1);
      A.Assert (Status, 42, "exit status should be 42");
   end if;

   Restore_Sigmask;

   return A.Report;
end Test;
