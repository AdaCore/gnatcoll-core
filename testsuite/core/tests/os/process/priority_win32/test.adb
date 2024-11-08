with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Test_Assert;
with Test_Python;
with GNAT.IO;

function Test return Integer is

   package A renames Test_Assert;
   package IO renames GNAT.IO;

begin
   IO.Put_Line ("GNATCOLL.OS.Process priorities high level interface");

   declare
      Args           : Argument_List;
      Status         : Integer;
      Output         : Unbounded_String;
      Base_Priority  : Integer;
      Final_Priority : Integer;
   begin
      IO.Put_Line ("test various values for priority");
      Args.Append (Test_Python.Python_Executable);
      Args.Append ("./check_priority.py");

      --  Assess current process priority
      Output := Run (Args, Priority => INHERIT,
                     Status => Status,
                     Strip => True);
      IO.Put_Line (To_String (Output));
      Base_Priority := Integer'Value (To_String (Output));
      IO.Put_Line ("process win32 priority is" & Base_Priority'Img);

      for Priority in Priority_Class'Range loop
         Output := Run (Args, Priority => Priority, Status => Status,
                        Strip => True);
         A.Assert
            (Status, 0,
             "python --version status be 0 (prio: " & Priority'Img & ")");
         Final_Priority := Integer'Value (To_String (Output));
         IO.Put_Line ("child unix priority is" & Final_Priority'Img);
         A.Assert (Final_Priority >= Base_Priority);

         --  The following test is based on the knowledge that we map unix
         --  priority 0 to NORMAL.
         if Base_Priority = 0 then
            case Priority is
               when HIGH =>
                  A.Assert (Final_Priority, 0, "cannot set priority to high");
               when ABOVE_NORMAL =>
                  A.Assert (Final_Priority, 0, "cannot set priority to high");
               when NORMAL | INHERIT =>
                  A.Assert (Final_Priority, 0,
                            "priority should stay to NORMAL");
               when BELOW_NORMAL | IDLE =>
                  A.Assert (Final_Priority > Base_Priority,
                            "we can lower priority");
            end case;
         end if;
      end loop;
   end;
   return A.Report;
end Test;
