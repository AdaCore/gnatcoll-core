with GNATCOLL.Traces; use GNATCOLL.Traces;
with Ada.Command_Line;

function Test return Integer is

   Log : constant Logger := Create ("PKG");

   procedure Foo (A : Integer);
   --  Recursive function that decrement A and that traces its value
   --  until it reaches 1 (included).

   procedure Foo (A : Integer) is
      Block_Log : constant Block_Logger := Create (Log);
   begin
      Trace (Log, "A =" & A'Img);
      if A > 1 then
         Foo (A - 1);
      end if;
   end Foo;

begin
   Parse_Config_File (Ada.Command_Line.Argument (1));
   Foo (3);

   Log.all.Assert
      (
         Condition          => True,
         Error_Message      => "[Test error msg]",
         Message_If_Success => "[Test success msg]",
         Raise_Exception    => False
      );

   Log.all.Assert
      (
         Condition          => False,
         Error_Message      => "[Test error msg]",
         Message_If_Success => "[Test success msg]",
         Raise_Exception    => False
      );
   return 0;
end Test;
