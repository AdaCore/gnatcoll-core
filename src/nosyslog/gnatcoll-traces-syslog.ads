------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
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
