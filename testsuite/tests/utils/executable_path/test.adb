with GNATCOLL.Utils;
with Test_Assert;
with Ada.Environment_Variables;
with Ada.Directories;
with GNAT.OS_Lib;

function Test return Integer is

   package A   renames Test_Assert;
   package GU  renames GNATCOLL.Utils;
   package Env renames Ada.Environment_Variables;
   package Dir renames Ada.Directories;
   package OS  renames GNAT.OS_Lib;

begin
   declare
      Exe_Path : constant String := GU.Executable_Path;
      Process_Status : Integer;
      Success : Boolean;
   begin
      A.Assert (OS.Is_Regular_File (Exe_Path));
      if not OS.Is_Directory ("bin") then
         Dir.Create_Directory ("bin");
         OS.Copy_File (Exe_Path, "bin", Success, Preserve => OS.Full);
         Process_Status := OS.Spawn ("bin/test", Args => (1 .. 0 => null));
         A.Assert (Process_Status = 0);
      end if;
      Dir.Set_Directory ("..");
      Env.Set ("PATH", "");
      A.Assert (Exe_Path, GU.Executable_Path,
                "Ensure that executable path is not impacted by " &
                "environment changes");
      A.Assert (OS.Is_Directory (GU.Executable_Location));
   end;

   return A.Report;
end Test;
