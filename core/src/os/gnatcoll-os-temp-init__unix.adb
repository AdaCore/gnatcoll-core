with Ada.Environment_Variables;
with GNATCOLL.OS.Stat;

separate (GNATCOLL.OS.Temp)
procedure Init is
   package Env renames Ada.Environment_Variables;
   package Stat renames GNATCOLL.OS.Stat;

   function Check (System_Dir : String) return Boolean;

   function Check (System_Dir : String) return Boolean
   is
   begin
      return System_Dir'Length > 0 and then
         Stat.Is_Directory (Stat.Stat (System_Dir));
   end Check;

begin
   declare
      TMPDIR : constant String := Env.Value ("TMPDIR", "");
   begin
      if Check (TMPDIR) then
         Startup_System_Temp_Dir := new UTF8.UTF_8_String'(TMPDIR);
         return;
      end if;
   end;

   declare
      TEMP : constant String := Env.Value ("TEMP", "");
   begin
      if Check (TEMP) then
         Startup_System_Temp_Dir := new UTF8.UTF_8_String'(TEMP);
         return;
      end if;
   end;

   declare
      TMP : constant String := Env.Value ("TMP", "");
   begin
      if Check (TMP) then
         Startup_System_Temp_Dir := new UTF8.UTF_8_String'(TMP);
         return;
      end if;
   end;

   if Check ("/tmp") then
      Startup_System_Temp_Dir := new UTF8.UTF_8_String'("/tmp");
      return;
   end if;

   if Check ("/var/tmp") then
      Startup_System_Temp_Dir := new UTF8.UTF_8_String'("/var/tmp");
      return;
   end if;

   if Check ("/usr/tmp") then
      Startup_System_Temp_Dir := new UTF8.UTF_8_String'("/usr/tmp");
      return;
   end if;

   raise OS_Error with "cannot find a suitable system dir for temporary files";
end Init;
