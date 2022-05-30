with GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;
with GNATCOLL.OS.Constants;
with GNATCOLL.OS; use GNATCOLL.OS;
with Test_Assert;
with Test_Python;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;

   procedure Test_Status (Emited_Status, Expected_Status : Integer);
   --  Create a subprocess that returns with a given status and check
   --  that we get the right status.

   procedure Test_Status (Emited_Status, Expected_Status : Integer) is
      Args   : OS.Process_Types.Arguments;
      Env    : OS.Process_Types.Environ;
      Status : Integer;
      Output : Unbounded_String;
   begin
      OS.Process_Types.Inherit (Env);

      OS.Process_Types.Add_Argument (Args, Test_Python.Python_Executable);
      OS.Process_Types.Add_Argument (Args, "-c");
      OS.Process_Types.Add_Argument
         (Args,
          "import sys; print('test status'); sys.exit(" &
          Emited_Status'Img & ")");

      Output := OS.Process.Run (Args, Env, Status => Status, Strip => True);
      A.Assert
         (Status, Expected_Status,
          Msg => "python -c ""import sys; sys.exit(...)"" should return " &
          Expected_Status'Img);
      A.Assert
         (To_String (Output), "test status",
          Msg => "expect 'test status'");
      OS.Process_Types.Deallocate (Args);
   end Test_Status;

begin
   IO.Put_Line ("GNATCOLL.OS.Process test exit status");
   Test_Status (0, 0);

   if GNATCOLL.OS.Constants.OS = Windows then
      Test_Status (2 ** 31 - 1, 2 ** 31 - 1);
   else
      Test_Status (2 ** 31 - 1, 255);
   end if;

   if GNATCOLL.OS.Constants.OS = Windows then
      Test_Status (-2 ** 31, -2 ** 31);
   else
      Test_Status (-2 ** 31, 0);
   end if;

   if GNATCOLL.OS.Constants.OS = Windows then
      Test_Status (-1, -1);
   else
      Test_Status (-1, 255);
   end if;

   Test_Status (1, 1);
   return A.Report;
end Test;
