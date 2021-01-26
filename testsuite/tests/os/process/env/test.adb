with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Env_Vars renames Ada.Environment_Variables;

begin
   IO.Put_Line ("GNATCOLL.OS.Process env high level interface");

   declare
      Args           : Argument_List;
      Env            : Environment_Dict;
      Status         : Integer;
      Output         : Unbounded_String;
   begin
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("./check_env.py");

      IO.Put_Line ("check case in which variable is inherited from parent");
      Env_Vars.Set ("VAR1", "parent_value");
      Output := Run (Args, Status => Status, Strip => True);
      A.Assert (To_String (Output), "parent_value");

      IO.Put_Line ("check case where variable should not be inherited");
      Output := Run (Args, Env => Env, Status => Status, Strip => True);
      A.Assert (To_String (Output), "None");

      IO.Put_Line ("check case where variable value is overriden using Env");
      Env.Include ("VAR1", "child_value");
      Output := Run (Args, Env => Env, Status => Status, Strip => True);
      A.Assert (To_String (Output), "child_value");
   end;

   return A.Report;
end Test;
