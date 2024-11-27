with GNATCOLL.Damerau_Levenshtein_Distance;
with Test_Assert;

function Test return Integer is
   package TA renames Test_Assert;

   function DLD (Left, Right : String) return Natural
              renames GNATCOLL.Damerau_Levenshtein_Distance;

   Sample : constant String := "0123456789abcdefgh";

begin
   TA.Assert (DLD (Sample, Sample), 0);
   TA.Assert (DLD (Sample, Sample (Sample'First .. Sample'Last - 1)), 1);
   TA.Assert (DLD (Sample, Sample (Sample'First + 1 .. Sample'Last - 1)), 2);
   TA.Assert (DLD (Sample, Sample & "xyz"), 3);
   TA.Assert (DLD ("0000" & Sample, Sample), 4);
   TA.Assert (DLD (Sample, "1023456789abcdefgh"), 1);
   TA.Assert (DLD (Sample, "1024356789abcdefgh"), 2);
   TA.Assert (DLD (Sample, "1v023456789abcdefgh"), 3);
   TA.Assert (DLD (Sample, "012345869abcdefgh"), 3);
   TA.Assert (DLD (Sample, "102456879abdcefgh"), 4);
   TA.Assert (DLD (Sample, "102345-6789-abcefgh"), 4);
   return TA.Report;
end Test;
