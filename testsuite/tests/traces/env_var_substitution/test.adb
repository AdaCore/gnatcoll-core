with GNATCOLL.Traces; use GNATCOLL.Traces;
with Test_Assert;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Environment_Variables;

function Test return Integer is
   H_PID : constant Trace_Handle := Create ("trace_pid");
   H_D   : constant Trace_Handle := Create ("trace_D");
   H_T   : constant Trace_Handle := Create ("trace_T");
   H_FOO : constant Trace_Handle := Create ("trace_FOO");
   function getpid return Integer;
   pragma Import (C, getpid, "getpid");

   Pid : constant String := getpid'Img;
   Pidfile : Ada.Text_IO.File_Type;
begin
   Create (Pidfile, Out_File, "tracename.pid");
   Put_Line (Pidfile, Pid);
   Close (Pidfile);

   Ada.Environment_Variables.Set ("FOO", "BAR");

   Parse_Config_File;
   Trace (H_PID, "hello from" & Pid);
   Trace (H_D, "hello from" & Pid);
   Trace (H_T, "hello from" & Pid);
   Trace (H_FOO, "hello from" & Pid);

   return Test_Assert.Report;
end Test;
