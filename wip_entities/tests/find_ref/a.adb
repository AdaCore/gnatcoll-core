with B;

package body A is
   procedure Proc1 (A, C : Integer; F : Float) is
   begin
      B.Proc1;
   end Proc1;

   procedure Proc2 is
   begin
      B.Proc1;
   end Proc2;
end A;
