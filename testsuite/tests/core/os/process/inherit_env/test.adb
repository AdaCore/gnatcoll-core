with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Test_Assert;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;
   package Env_Vars renames Ada.Environment_Variables;
   package PT renames GNATCOLL.OS.Process_Types;

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
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         A.Assert (To_String (Output), "<novalue>" & ASCII.LF & "<novalue>");

         IO.Put_Line ("check case in which variable is in parent env");
         Env_Vars.Set ("VAR1", "parent_value");
         Env_Vars.Set ("VAR2", "parent_value2");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         A.Assert
            (To_String (Output), "parent_value" & ASCII.LF & "parent_value2");

         IO.Put_Line ("check case in which variable is redefined");
         Env.Include ("VAR1", "arg_value");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         A.Assert
            (To_String (Output), "arg_value" & ASCII.LF & "parent_value2");

         Env.Include ("VAR2", "arg_value2");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         A.Assert
            (To_String (Output), "arg_value" & ASCII.LF & "arg_value2");

         --  One of the following two tests will fail if case insensitive
         --  case is not well implemented.
         Env.Include ("Var2", "arg_value2_2");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         if PT.Case_Sensitive_Env_Var_Names then
            A.Assert
               (To_String (Output), "arg_value" & ASCII.LF & "arg_value2");
         else
            A.Assert
               (To_String (Output), "arg_value" & ASCII.LF & "arg_value2_2");
         end if;

         Env.Include ("VAR2", "arg_value2");
         Output := Run
            (Args, Env => Env, Status => Status,
             Strip => True, Inherit_Env => True,
             Universal_Newline => True);
         A.Assert
            (To_String (Output), "arg_value" & ASCII.LF & "arg_value2");

      end;
      return A.Report;
   else
      IO.Put_Line (Env_Vars.Value ("VAR1", "<novalue>"));
      IO.Put_Line (Env_Vars.Value ("VAR2", "<novalue>"));
      return 0;
   end if;
end Test;
