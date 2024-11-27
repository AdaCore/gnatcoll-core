with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO;

package body Test_Measure is
   Start : Time;

   procedure Start_Measure is
   begin
      Start := Clock;
   end Start_Measure;

   procedure End_Measure (Message : String; Compare_With : Duration := 0.0)
   is
      Test_Time : constant Duration := Clock - Start;
   begin
      if Compare_With > 0.0 then
         declare
            Ratio : constant Long_Float :=
              Long_Float (Test_Time) / Long_Float (Compare_With) * 100.0;
         begin
            Ada.Text_IO.Put_Line
               (Message & ":" &
                Integer (Ratio)'Img & "% compared to baseline");
         end;
      end if;
      Ada.Text_IO.Put_Line (Message & ":" & Test_Time'Img & "s total time");
   end End_Measure;
end Test_Measure;
