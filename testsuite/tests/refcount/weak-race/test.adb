with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Real_Time;             use Ada.Real_Time;
with Ada.Task_Identification;
with Ada.Text_IO;               use Ada.Text_IO;
with GNATCOLL.Refcount;
with System.Address_Image;
with Test_Assert;               use Test_Assert;

function Test return Integer is
   package Shared_Holders is new GNATCOLL.Refcount.Shared_Pointers (Integer);
   use Shared_Holders;

   task type Weak_Tester is
      entry Take (Ptr : Ref);
      entry Stop;
   end Weak_Tester;

   subtype Processing_Range is Integer range -10_000 .. 10_000;

   task body Weak_Tester is
      Stop_It : Boolean := False;
      W : Weak_Ref;
   begin
      Main_Loop : for J in Processing_Range loop
         declare
            R : Ref;
         begin
            select
               accept Take (Ptr : Ref) do
                  R := Ptr;
               end Take;
            or
               accept Stop do
                  Stop_It := True;
               end Stop;
            end select;

            W := R.Weak;
         end;

         exit Main_Loop when Stop_It;

         for K in Natural'Range loop
            declare
               use Ada.Task_Identification;
               R : Ref;
            begin
               R.Set (W);
               if R = Null_Ref then
                  if K = 0 then
                     Put_Line
                       ("Taken null at first time " & J'Img & ' '
                        & Image (Current_Task));
                  end if;

                  if J rem 9999 = 0 then
                     Put_Line (J'Img & K'Img);
                  end if;

                  exit;
               end if;

               if R.Get /= J then
                  Assert
                    (False,
                     "Expected " & J'Img & " took " & Integer'(R.Get)'Img);
               end if;
            end;
         end loop;
      end loop Main_Loop;
   exception
      when E : others =>
         Assert (False, "Task " & Exception_Information (E));
   end Weak_Tester;

   Test  : array (1 .. 3) of Weak_Tester;
   Stamp : Time := Clock;

begin
   for J in Processing_Range loop
      declare
         R : Ref;
      begin
         R.Set (J);

         for J in Test'Range loop
            Test (J).Take (R);
         end loop;

         delay 0.00001;
      end;

      if To_Duration (Clock - Stamp) > 200.0 then
         Put_Line ("Too busy test machine, stop the test.");

         for J in Test'Range loop
            Test (J).Stop;
         end loop;

         exit;
      end if;
   end loop;

   return Test_Assert.Report;

exception
   when E : others =>
      Assert (False, "Main " & Exception_Information (E));
      for J in Test'Range loop
         Test (J).Stop;
      end loop;
   return Test_Assert.Report;
end Test;
