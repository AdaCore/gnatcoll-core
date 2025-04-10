with Mylib;
with Test_Assert;

function Test return Integer is
begin
   Mylib.Run;
   return Test_Assert.Report;
end Test;
