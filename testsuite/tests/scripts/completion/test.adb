with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Shell; use GNATCOLL.Scripts.Shell;
with TestConsole;            use TestConsole;
with Support;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   Repo    : Scripts_Repository := new Scripts_Repository_Record;
   Console : aliased Test_Console;

   function Completions
      (Input : String;
       Lang  : Scripting_Language)
      return String;

   function Completions
      (Input : String;
       Lang  : Scripting_Language)
     return String
   is
      use String_Lists;
      Completions : String_Lists.List;
      Result      : Unbounded_String;
   begin
      Complete (Lang, Input, Completions);
      for E of Completions loop
         Append (Result, E & ", ");
      end loop;
      return To_String (Result);
   end Completions;

   Sh : Scripting_Language;

begin
   Register_Shell_Scripting (Repo);
   Register_Standard_Classes (Repo, "Console");

   Support.Register_Functions (Repo);

   Sh := Lookup_Scripting_Language (Repo, "shell");

   Set_Default_Console (Sh, Console'Unchecked_Access);

   A.Assert (Completions ("C1", Sh) = "C1, C1.method, ", "C1");
   A.Assert (Completions ("", Sh) =
             "C1, C1.method, Console.clear, Console.flush, Console.isatty, " &
             "Console.read, Console.readline, Console.write, MyList.dump, " &
             "clear_cache, echo, echo_error, hello, load, print_float, ",
             """""");
   A.Assert (Completions ("Ba", Sh) = "", "Ba");

   Free (Console);
   Destroy (Repo);

   return A.Report;
end Test;
