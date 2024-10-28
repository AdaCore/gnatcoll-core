with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   BT : constant Boolean := True;
   BF : constant Boolean := False;

   T : constant Triboolean := True;
   F : constant Triboolean := False;
   I : constant Triboolean := Indeterminate;
begin

   ----------------
   --  Equality  --
   ----------------

   A.Assert (BT = T, "BT=T");
   A.Assert (Equal (BT, T) = Triboolean'(True), "BT eq T");

   A.Assert (not (BT = F), "BT=F");
   A.Assert (Equal (BT, F) = Triboolean'(False), "BT eq F");

   A.Assert (not (BT = I), "BT=I");
   A.Assert (Equal (BT, I) = Indeterminate, "BT eq I");

   A.Assert (not (BF = T), "BF=T");
   A.Assert (Equal (BF, T) = Triboolean'(False), "BF eq F");

   A.Assert (BF = F, "BF=F");
   A.Assert (Equal (BF, F) = Triboolean'(True), "BF eq F");

   A.Assert (not (BF = I), "BF=I");
   A.Assert (Equal (BF, I) = Indeterminate, "BF eq I");

   -----------
   --  And  --
   -----------

   A.Assert ((BF and F) = Triboolean'(False), "BF and F");
   A.Assert ((BF and T) = Triboolean'(False), "BF and T");
   A.Assert ((BF and I) = Triboolean'(False), "BF and I");
   A.Assert ((BT and F) = Triboolean'(False), "BT and F");
   A.Assert ((BT and T) = Triboolean'(True), "BT and T");
   A.Assert ((BT and I) = Indeterminate, "BT and I");
   A.Assert ((T and F) = Triboolean'(False), "T and F");
   A.Assert ((T and T) = Triboolean'(True), "T and T");
   A.Assert ((T and I) = Indeterminate, "T and I");
   A.Assert ((F and F) = Triboolean'(False), "F and F");
   A.Assert ((F and T) = Triboolean'(False), "F and T");
   A.Assert ((F and I) = Triboolean'(False), "F and I");
   A.Assert ((I and F) = Triboolean'(False), "I and F");
   A.Assert ((I and T) = Indeterminate, "I and T");
   A.Assert ((I and I) = Indeterminate, "I and I");

   ----------
   --  Or  --
   ----------

   A.Assert ((BF or F) = Triboolean'(False), "BF or F");
   A.Assert ((BF or T) = Triboolean'(True), "BF or T");
   A.Assert ((BF or I) = Indeterminate, "BF or I");
   A.Assert ((BT or F) = Triboolean'(True), "BT or F");
   A.Assert ((BT or T) = Triboolean'(True), "BT or T");
   A.Assert ((BT or I) = Triboolean'(True), "BT or I");
   A.Assert ((T or F) = Triboolean'(True), "T or F");
   A.Assert ((T or T) = Triboolean'(True), "T or T");
   A.Assert ((T or I) = Triboolean'(True), "T or I");
   A.Assert ((F or F) = Triboolean'(False), "F or F");
   A.Assert ((F or T) = Triboolean'(True), "F or T");
   A.Assert ((F or I) = Indeterminate, "F or I");
   A.Assert ((I or F) = Indeterminate, "I or F");
   A.Assert ((I or T) = Triboolean'(True), "I or T");
   A.Assert ((I or I) = Indeterminate, "I or I");

   -----------
   --  Xor  --
   -----------

   A.Assert ((BF xor F) = Triboolean'(False), "BF xor F");
   A.Assert ((BF xor T) = Triboolean'(True), "BF xor T");
   A.Assert ((BF xor I) = Indeterminate, "BF xor I");
   A.Assert ((BT xor F) = Triboolean'(True), "BT xor F");
   A.Assert ((BT xor T) = Triboolean'(False), "BT xor T");
   A.Assert ((BT xor I) = Indeterminate, "BT xor I");
   A.Assert ((T xor F) = Triboolean'(True), "T xor F");
   A.Assert ((T xor T) = Triboolean'(False), "T xor T");
   A.Assert ((T xor I) = Indeterminate, "T xor I");
   A.Assert ((F xor F) = Triboolean'(False), "F xor F");
   A.Assert ((F xor T) = Triboolean'(True), "F xor T");
   A.Assert ((F xor I) = Indeterminate, "F xor I");
   A.Assert ((I xor F) = Indeterminate, "I xor F");
   A.Assert ((I xor T) = Indeterminate, "I xor T");
   A.Assert ((I xor I) = Indeterminate, "I xor I");

   -----------
   --  Not  --
   -----------

   A.Assert (not F = Triboolean'(True), "not F");
   A.Assert (not T = Triboolean'(False), "not T");
   A.Assert (not I = Indeterminate, "not I");

   return A.Report;

end Test;
