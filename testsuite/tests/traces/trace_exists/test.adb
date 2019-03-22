with Test_Assert;
with GNATCOLL.Traces; use GNATCOLL.Traces;

function Test return Integer
is
   Name  : constant String := "FooBar";
   Trace : Trace_Handle;
begin
   Test_Assert.Assert (not Exists (Name), "The trace should not be defined");
   Trace := Create (Name, Off);
   Test_Assert.Assert (not Active (Trace), "The trace should be disabled");
   Test_Assert.Assert (Exists (Name), "The trace should be defined");
   Trace := Create (Name, On);
   Test_Assert.Assert (not Active (Trace),
                       "The trace should still be disabled");
   Test_Assert.Assert (Exists (Name), "The trace should be defined");
   return Test_Assert.Report;
end Test;
