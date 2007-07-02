------------------------------------------------------------------------------
--                               G N A T L I B                              --
--                                                                          --
--                      Copyright (C) 2001-2007, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

with GNAT.Traces; use GNAT.Traces;
with GNAT.IO;     use GNAT.IO;

procedure Traces1 is
   Me1 : GNAT.Traces.Trace_Handle := Create ("MODULE1");
   Me2 : GNAT.Traces.Trace_Handle := Create ("MODULE2", On);
   Me3 : GNAT.Traces.Trace_Handle := Create ("MODULE3", Off);
   Me4 : GNAT.Traces.Trace_Handle;
begin
   --  No config file parsed => all streams go to stdout by default.
   --  All streams are disabled
   if Active (Me1) then
      Put_Line ("Error: streams should be disabled by default");
   end if;
   if not Active (Me2) then
      Put_Line ("Error: stream2 is explicitely set to On");
   end if;
   if Active (Me3) then
      Put_Line ("Error: stream3 is explicitely set to Off");
   end if;

   Trace (Me1, "Message 1");
   Trace (Me2, "Message 2");
   Trace (Me3, "Message 3");

   --  If another module redefines the same traces, overriding the
   --  status, we should change only the ones using the default status
   --  so that the order of elaboration of modules is irrelevant for traces

   Me1 := Create ("MODULE1", On);
   Me2 := Create ("MODULE2", Off);  --  No effect
   Me3 := Create ("MODULE3", On);   --  No effect

   if not Active (Me1) then
      Put_Line ("Error: stream1 was explicitely activated");
   end if;
   if not Active (Me2) then
      Put_Line ("Error: stream2 is explicitely set to On, can't override");
   end if;
   if Active (Me3) then
      Put_Line ("Error: stream3 is explicitely set to Off, can't override");
   end if;

   Trace (Me1, "Message 1");
   Trace (Me2, "Message 2");
   Trace (Me3, "Message 3");

   --  Explicitly changing activation status

   Set_Active (Me1, False);
   Set_Active (Me2, False);
   Set_Active (Me3, True);

   if Active (Me1) then
      Put_Line ("Error: stream1 was explicitely deactivated");
   end if;
   if Active (Me2) then
      Put_Line ("Error: stream2 was explicitely deactivated");
   end if;
   if not Active (Me3) then
      Put_Line ("Error: stream3 was explicitely activated");
   end if;

   --  Testing asserts

   Assert (Me3, False,
           Error_Message      => "Always False",
           Message_If_Success => "Assert succeeded",
           Raise_Exception    => False);
   Assert (Me3, True,
           Error_Message      => "Always False",
           Message_If_Success => "Assert succeeded",
           Raise_Exception    => False);

   --  Testing indentation, should apply to all streams

   Set_Active (Me1, True);
   Set_Active (Me2, True);

   Increase_Indent (Me1, "Increase indent");
   Trace (Me2, "Standard trace");
   Increase_Indent;
   Trace (Me3, "Another trace");
   Decrease_Indent;
   Trace (Me2, "Standard trace");
   Decrease_Indent (Me1);

   --  Redirecting to another stream

   Me4 := Create ("MODULE4", On, Stream => "&2");
   Trace (Me4, "To stderr ?");

   --  Dumping the current configuration

   New_Line;
   Show_Configuration (GNAT.IO.Put_Line'Access);

   --  Logging after finalizing the module

   Finalize;
   Trace (Me1, "After finalization");

end Traces1;
