------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
--  This package provides two levels of interfaces:
--     - a low level interface to syslog (on Unix systems)
--     - a higher level interface that can be used through GNAT.Traces.
--  syslog is the system logger on Unix systems.

package GNATCOLL.Traces.Syslog is

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
      Sys_Log,   -- messages generated internally
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

end GNATCOLL.Traces.Syslog;
