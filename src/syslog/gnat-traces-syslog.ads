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

--  Interface to syslog.
--  This package provides two levels of interfaces:
--     - a low level interface to syslog (on Unix systems)
--     - a higher level interface that can be used through GNAT.Traces.
--  syslog is the system logger on Unix systems.

package GNAT.Traces.Syslog is

   Stream_Syslog : constant String := "syslog";
   --  Name of the stream that can be used in GNAT.Traces configuratino files
   --  or calls to Create to send a stream to syslog.
   --  You must have called Register_Syslog_Stream (see below) first.

   procedure Register_Syslog_Stream;
   --  Register a GNAT.Traces stream that can send its output to the system
   --  logger syslog. This stream takes two optional arguments which are the
   --  facility and the level to pass to calls to Syslog (see below).
   --  For instance, your configuration file for GNAT.Traces could contains
   --      SYSLOG_ERROR=yes >&syslog:local0:error
   --      SYSLOG_INFO=yes >&syslog:local0:info
   --  and then your Ada code can use:
   --      Errors : Trace_Handle := Create ("SYSLOG_ERROR");
   --      Info   : Trace_Handle := Create ("SYSLOG_INFO");
   --      Trace (Errors, "An error");
   --  to send messages to syslog. Since GNAT.Traces can be configured
   --  dynamically, this means that the Errors stream defined above could be
   --  redirected for instance to stdout instead on systems where syslog is not
   --  supported.

   -----------------------------------
   -- Low-level interface to syslog --
   -----------------------------------
   --  The following types and subprograms can be used if you need to interface
   --  directly to syslog. One drawback is that these are not usable (will not
   --  even exist) on systems that do not have syslog.
   --
   --  A message sent to syslog has two attributes: its facility, and its
   --  level. The facility indicates what type of program is logging the
   --  message. This lets the syslog configuration file (system-wide) specify
   --  that messages from different facilities will be handled differently.
   --  The level determines the importance of the message.

   type Levels is
     (Emergency,  -- system is unusable
      Alert,      -- action must be taken immediately
      Critical,   -- critical conditions
      Error,      -- error conditions
      Warning,    -- warning conditions
      Notice,     -- normal but significant condition
      Info,        -- informational
      Debug);     -- debug-level messages
   --  Importance of the messages, in order of decreasing importance

   type Facilities is
     (Kernel,    -- kernel messages
      User,      -- random user-level messages
      Mail,      -- mail system
      Daemon,    -- system daemons
      Auth,      -- security/authorization messages
      Syslog,    -- messages generated internally
      Printer,   -- line printer subsystem
      News,      -- network news subsystem
      UUCP,      -- UUCP subsystem
      Cron,      -- clock daemon
      Auth_Priv, -- security/authorization messages
      FTP,       -- ftp daemon
      NTP,       -- ntp daemon
      Security,  -- security subsystems
      Console,   -- /dev/console output
      Local0,    -- reserved for local use
      Local1,    -- reserved for local use
      Local2,    -- reserved for local use
      Local3,    -- reserved for local use
      Local4,    -- reserved for local use
      Local5,    -- reserved for local use
      Local6,    -- reserved for local use
      Local7);   -- reserved for local use
   --  What type of program is logging the message

   type Options is mod Integer'Last;
   --  Options when opening the connection to syslog

   None       : constant Options; -- no options at all
   PID        : constant Options; -- log the pid with each message
   Cons       : constant Options; -- log on the console if errors
   Open_Delay : constant Options; -- delay open() until first call to syslog()
   No_Delay   : constant Options; -- don't delay open()
   No_Wait    : constant Options; -- don't wait for console forks
   Std_Error  : constant Options; -- log to stderr as well

   procedure Openlog
     (Prefix           : String;
      Customization    : Options;
      Default_Facility : Facilities);
   --  The (optional) call to this subprogram specifies the attributes of the
   --  connection to syslog. In particular, Prefix will be prepended to every
   --  message, and is in general used to specify the name of the program.
   --  Customization specifies flags to control the connection (in particular
   --  whether the PID of the process should be logged).
   --  Finally, Default_Facility will be used when the call to Syslog (see
   --  below) does not specify the facility.

   procedure Syslog
     (Facility : Facilities := Kernel;
      Level    : Levels     := Emergency;
      Message  : String);
   --  Writes Message to the system logger. If Facility is left to its default
   --  value, the priority specified in the call to Openlog will be used
   --  instead.

   procedure Closelog;
   --  The (optional) call to this subprogram closes the connection with syslog

private
   None       : constant Options := 16#00#;
   PID        : constant Options := 16#01#;
   Cons       : constant Options := 16#02#;
   Open_Delay : constant Options := 16#04#;
   No_Delay   : constant Options := 16#08#;
   No_Wait    : constant Options := 16#10#;
   Std_Error  : constant Options := 16#20#;

end GNAT.Traces.Syslog;
