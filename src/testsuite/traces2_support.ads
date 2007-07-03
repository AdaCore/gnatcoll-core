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

with GNAT.Strings; use GNAT.Strings;
with GNAT.Traces;  use GNAT.Traces;

package Traces2_Support is

   type My_Stream is new GNAT.Traces.Trace_Stream_Record with record
      Args : GNAT.Strings.String_Access;
   end record;
   overriding procedure Put (Stream : in out My_Stream; Str : String);
   overriding procedure Newline (Stream : in out My_Stream);

   function Factory (Args : String) return Trace_Stream;

   type My_Handle is new GNAT.Traces.Trace_Handle_Record with null record;
   overriding procedure Pre_Decorator
     (Handle  : in out My_Handle;
      Stream  : in out Trace_Stream_Record'Class;
      Message : String);
   overriding procedure Post_Decorator
     (Handle   : in out My_Handle;
      Stream   : in out Trace_Stream_Record'Class;
      Location : String;
      Entity   : String;
      Message  : String);

   function My_Handle_Factory return Trace_Handle;

end Traces2_Support;
