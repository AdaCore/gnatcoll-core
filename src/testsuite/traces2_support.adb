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

with GNAT.IO;     use GNAT.IO;

package body Traces2_Support is

   procedure Put (Stream : in out My_Stream; Str : String) is
   begin
      Put_Line (Stream.Args.all & "--" & Str & "--");
   end Put;

   procedure Newline (Stream : in out My_Stream) is
   begin
      Put_Line (Stream.Args.all & "--------------------");
   end Newline;

   function Factory (Args : String) return Trace_Stream is
   begin
      return new My_Stream'
         (Trace_Stream_Record with Args => new String'(Args));
   end Factory;

   procedure Pre_Decorator
     (Handle  : in out My_Handle;
      Stream  : in out Trace_Stream_Record'Class;
      Message : String) is
   begin
      Put (Stream, "[PREDECORATOR]");
      Pre_Decorator (Trace_Handle_Record (Handle), Stream, Message);
   end Pre_Decorator;

   procedure Post_Decorator
     (Handle   : in out My_Handle;
      Stream   : in out Trace_Stream_Record'Class;
      Location : String;
      Entity   : String;
      Message  : String) is
   begin
      Put (Stream, "[POSTDECORATOR]");
      Post_Decorator
        (Trace_Handle_Record (Handle), Stream, Location, Entity, Message);
   end Post_Decorator;

   function My_Handle_Factory return Trace_Handle is
   begin
      return new My_Handle;
   end My_Handle_Factory;

end Traces2_Support;
