------------------------------------------------------------------------------
--                               G N A T C O L L                            --
--                                                                          --
--                      Copyright (C) 2001-2008, AdaCore                    --
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

--  Interface to syslog.
--  These are only stubs, because syslog is not supported on this system

package GNATCOLL.Traces.Syslog is

   Stream_Syslog : constant String := "syslog";
   --  Name of the stream that can be used in GNAT.Traces configuratino files
   --  or calls to Create to send a stream to syslog.
   --  You must have called Register_Syslog_Stream (see below) first.

   procedure Register_Syslog_Stream;
   --  Stub, to make this package compatible with systems that do have
   --  syslog. In the current case, you can still send traces to the "syslog"
   --  stream, and that will in fact display them on the default output
   --  stream as configured in the configuration file for GNAT.Traces.

end GNATCOLL.Traces.Syslog;
