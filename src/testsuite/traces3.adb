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

with GNAT.Traces;        use GNAT.Traces;
with GNAT.Traces.Syslog; use GNAT.Traces.Syslog;

procedure Traces3 is
   Colors : constant Trace_Handle := Create ("DEBUG.COLORS");
   Time   : constant Trace_Handle := Create ("DEBUG.ABSOLUTE_TIME");
   Me1 : GNAT.Traces.Trace_Handle;
   Me2 : GNAT.Traces.Trace_Handle;
   Me3 : GNAT.Traces.Trace_Handle;
begin
   Set_Active (Colors, True);
   Set_Active (Time, True);
   Register_Syslog_Stream;
   Me1 := Create ("MODULE1", On, "&syslog");
   Me2 := Create ("MODULE2", On, "&syslog:local0:info");
   Me3 := Create ("MODULE2", On, "&syslog:local0:error");

   Trace (Me1, "Message 1");
   Trace (Me2, "Message 2");
   Trace (Me3, "Message 3");
end Traces3;
