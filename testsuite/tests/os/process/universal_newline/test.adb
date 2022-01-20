with GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;
with GNATCOLL.OS.FS;
with GNATCOLL.OS;
with GNATCOLL.OS.Constants;
with Test_Assert;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Text_IO;

function Test return Integer is
   package A renames Test_Assert;
   package IO renames Ada.Text_IO;
   package OS renames GNATCOLL.OS;
   package PT renames GNATCOLL.OS.Process_Types;
   package FS renames GNATCOLL.OS.FS;

   use type GNATCOLL.OS.OS_Type;

   Msg : constant String := "a" & ASCII.CR & ASCII.LF & "b" & ASCII.CR & "c";

   Non_Filtered_Out : constant String := (
      if OS.Constants.OS = OS.Windows then
         "a" & ASCII.CR & ASCII.CR & ASCII.LF & "b" & ASCII.CR & "c"
      else
         "a" & ASCII.CR & ASCII.LF & "b" & ASCII.CR & "c");

   Filtered_Out : constant String := (
      if OS.Constants.OS = OS.Windows then
         "a" & ASCII.CR & ASCII.LF & "b" & ASCII.CR & "c"
      else
         "a" & ASCII.LF & "b" & ASCII.CR & "c");

   procedure Test_IO (Mode : String);

   procedure Test_IO (Mode : String) is
      Args   : PT.Arguments;
      Env    : PT.Environ;
      Status : Integer;
      Output : Unbounded_String;
   begin
      IO.Put_Line ("== Test buffered IO mode=" & Mode & " ==");
      PT.Add_Argument (Args, Ada.Command_Line.Command_Name);
      PT.Inherit (Env);

      --  This the topelevel program that check redirections
      PT.Add_Argument (Args, Mode);

      Output := OS.Process.Run (Args, Env, Status => Status);
      A.Assert (Status, 0, Msg => "program should return 0 as status");
      A.Assert (To_String (Output), Non_Filtered_Out, "check_output un=False");

      Output := OS.Process.Run
         (Args, Env, Status => Status, Universal_Newline => True);
      A.Assert (Status, 0, Msg => "program should return 0 as status");
      A.Assert (To_String (Output), Filtered_Out, "check_output un=True");
   end Test_IO;

begin

   if Ada.Command_Line.Argument_Count = 0 then
      Test_IO ("non_buffered_io");
      Test_IO ("buffered_io");
      return A.Report;

   elsif Ada.Command_Line.Argument (1) = "buffered_io" then
      --  Don't use Ada.Text_IO as some trailing LF is always added.
      FS.Write (FS.Standout, Msg);

   elsif Ada.Command_Line.Argument (1) = "non_buffered_io" then
      --  Emit characters one by one and put delays between them. In
      --  OS.Process this ensure we test the case in which not all output is
      --  received at once.
      for J in Msg'Range loop
         FS.Write (FS.Standout, Msg (J) & "");
         IO.Flush;
         delay 0.05;
      end loop;
   end if;

   return 0;
end Test;
