
with GNAT.Strings;

with GNATCOLL.Traces;
with GNATCOLL.VFS;
with Test_Assert;

function Test return Integer is
   Me : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("TEST");

begin
   GNATCOLL.Traces.Parse_Config
     ("+" & ASCII.LF
      & ">log" & ASCII.LF);

   Me.Trace ("Log");

   declare
      use type GNAT.Strings.String_Access;

      Log  : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create ("log");
      Data : GNAT.Strings.String_Access;
   begin
      Data := Log.Read_File;

      Test_Assert.Assert (Data /= null, "should not be null");

      GNAT.Strings.Free (Data);
   end;

   return Test_Assert.Report;
end Test;
