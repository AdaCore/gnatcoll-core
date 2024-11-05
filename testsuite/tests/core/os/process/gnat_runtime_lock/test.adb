with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.OS;
with Test_Assert;
with Test_Python;
with Ada.Exceptions;
with GNAT.IO;
with GNAT.Task_Lock;
with GNATCOLL.OS.FS;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;

   task User_Task is
      entry Start_Task;
      entry End_Task;
   end User_Task;

   task body User_Task is
   begin
      accept Start_Task do
         IO.Put_Line ("Task starts");
      end Start_Task;

      GNAT.Task_Lock.Lock;
      GNAT.Task_Lock.Unlock;

      accept End_Task do
         IO.Put_Line ("Task ends");
      end End_Task;
   end User_Task;

begin
   IO.Put_Line
      ("Check that GNATCOLL.OS.Process release runtime lock in case of error");

   declare
      Args           : Argument_List;
      Status         : Integer;
   begin
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("--version");
      begin
         Status := Run (Args, Stdin => OS.FS.Invalid_FD);
         A.Assert
            (False, "process should not run (got status:" & Status'Img & ")");
      exception
         when E : OS.OS_Error =>
            A.Assert
               (True,
                "got expected exception OS_Error: " &
                Ada.Exceptions.Exception_Message (E));
      end;
      Args.Clear;

      Args.Append (Test_Python.Python_Executable & "-non-existant");
      Args.Append ("--version");
      begin
         Status := Run (Args);
         --  On some system we may just get a return status of 127
         A.Assert
            (Status, 127,
             "process should not run (got status:" & Status'Img & ")");
      exception
         when E : OS.OS_Error =>
            A.Assert
               (True,
                "got expected exception OS_Error: " &
                Ada.Exceptions.Exception_Message (E));
      end;
      Args.Clear;

      IO.Put_Line ("Check that GNAT lock can be acquired from another task");
      User_Task.Start_Task;
      User_Task.End_Task;
      A.Assert (True, "lock acquired and released");
   end;

   return A.Report;
end Test;
