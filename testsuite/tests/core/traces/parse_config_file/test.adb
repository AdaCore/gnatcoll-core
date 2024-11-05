with GNATCOLL.Traces;
with Test_Assert;

function Test return Integer is
   package Traces renames GNATCOLL.Traces;
   package A renames Test_Assert;
begin
   Traces.Parse_Config_File ("my.cfg");
   A.Assert (True, "should not raise an exception");
   return A.Report;
end Test;
