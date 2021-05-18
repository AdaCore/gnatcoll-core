with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.OS;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package OS renames GNATCOLL.OS;

begin
   IO.Put_Line ("GNATCOLL.OS.Process args high level interface");

   declare
      Args           : Argument_List;
      Status         : Integer;
      Output         : Unbounded_String;
   begin
      IO.Put_Line ("Check invocation of non existing executable");
      Args.Append (Test_Python.Python_Executable & "-non-existant");
      Args.Append ("--version");
      begin
         Status := Run (Args);
         A.Assert
            (Status, 127,
             "if no exception is raised then status should 127. status:" &
             Status'Img);
      exception
         when OS.OS_Error =>
            A.Assert (True, "got expected exception OS_Error");
      end;
      Args.Clear;

      IO.Put_Line ("Check invocation of existing executable");
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("./check_args.py");
      Output := Run (Args, Status => Status, Strip => True);
      A.Assert (To_String (Output), "0|");

      IO.Put_Line ("Check invocation of existing executable with parameters");
      Args.Append ("-v");
      Args.Append ("""");
      Args.Append ("'");
      Args.Append ("" & ASCII.LF & "a");
      Args.Append ("\a");
      Output := Run (Args, Status => Status, Strip => True);
      A.Assert (To_String (Output), "5|-v|""|'|\na|\a");

      IO.Put_Line ("Check utf-8 support");
      Args.Clear;
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("./check_args.py");
      Args.Append ("ハ");
      Output := Run (Args, Status => Status, Strip => True);
      A.Assert (To_String (Output), "1|ハ");
   end;

   return A.Report;
end Test;
