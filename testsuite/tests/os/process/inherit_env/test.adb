with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Test_Assert;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Env_Vars renames Ada.Environment_Variables;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      IO.Put_Line ("GNATCOLL.OS.Process inherit_env parameter");

      declare
         Args           : Argument_List;
         Env            : Environment_Dict;
         Status         : Integer;
         Output         : Unbounded_String;
      begin
         Args.Append (Ada.Command_Line.Command_Name);
         Args.Append ("subtest");

         --  The following variables should be preserved in to be able to
         --  execute the test
         Env.Include ("PATH", Env_Vars.Value ("PATH"));
         Env.Include
            ("LD_LIBRARY_PATH", Env_Vars.Value ("LD_LIBRARY_PATH", ""));

         IO.Put_Line ("check case in which variable is not present");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True);
         A.Assert (To_String (Output), "<novalue>");

         Env_Vars.Set ("VAR1", "parent_value");
         IO.Put_Line ("check case in which variable is in parent env");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True);
         A.Assert (To_String (Output), "parent_value");

         Env.Include ("VAR1", "argument_value");
         IO.Put_Line ("check case in which variable is redefined");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True);
         A.Assert (To_String (Output), "argument_value");

      end;
      return A.Report;
   else
      IO.Put_Line (Env_Vars.Value ("VAR1", "<novalue>"));
      return 0;
   end if;
end Test;
