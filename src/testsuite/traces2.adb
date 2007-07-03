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

with GNAT.Traces;     use GNAT.Traces;
with Traces2_Support; use Traces2_Support;

procedure Traces2 is
   Me1 : GNAT.Traces.Trace_Handle;
   Me2 : GNAT.Traces.Trace_Handle;
   Me3 : GNAT.Traces.Trace_Handle;
   Me4 : GNAT.Traces.Trace_Handle;
begin
   Register_Stream_Factory ("mystream", Factory'Access);

   --  Output to custom stream

   Me1 := Create ("MODULE1", Stream => "&mystream:me1");
   Me2 := Create ("MODULE2");  --  Read before config file
   Parse_Config_File ("traces_config.cfg");
   Me3 := Create ("MODULE3");  --  Read after config file
   Me4 := Create ("MODULE4");  --  Use default stream

   Trace (Me1, "Message 1");
   Trace (Me2, "Message 2");
   Trace (Me3, "Message 3");
   Trace (Me4, "Message 4");

   --  Overriding stream has no effect when the stream is already
   --  associated with a non-default stream

   Me1 := Create ("MODULE1", Stream => "&mystream:overridden");
   Trace (Me1, "Message to overridden stream");

   --  ..  but has an effect on default streams

   Me4 := Create ("MODULE4", Stream => "&mystream:overridden");
   Trace (Me4, "Message to overridden stream");

   --  Output to file

   Me1 := Create ("MODULE5", Stream => "outfile");
   Trace (Me1, "Message 1 to outfile");

   --  output to non-existing stream (defaults to default stream)

   Me1 := Create ("MODULE6", Stream => "&invalid");
   Trace (Me1, "Message 1 to invalid stream");

   --  Custom handles

   Set_Active (Create ("DEBUG.COUNT"), True);
   Set_Active (Create ("DEBUG.ENCLOSING_ENTITY"), True);
   Me1 := Create ("CUSTOM",
                  Stream => "&1",
                  Factory => My_Handle_Factory'Access);
   Trace (Me1, "Message on custom");

   Finalize;

end Traces2;
