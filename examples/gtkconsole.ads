-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides an example of a graphical textual window that
--  can be used as a console for a programming language

with Gtk.Handlers;
with Gtk.Scrolled_Window;
with Gtk.Text_Mark;
with Gtk.Text_View;
with Gtk.Widget;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;

package GtkConsole is

   type Gtk_Console_Record is new Virtual_Console_Record with private;
   type Gtk_Console is access all Gtk_Console_Record'Class;

   overriding procedure Ref   (Console : access Gtk_Console_Record);
   overriding procedure Unref (Console : access Gtk_Console_Record);
   overriding procedure Insert_Text
     (Console : access Gtk_Console_Record; Txt : String);
   overriding procedure Insert_Log
     (Console : access Gtk_Console_Record; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Gtk_Console_Record; Txt : String);
   overriding procedure Insert_Error
     (Console : access Gtk_Console_Record; Txt : String);
   overriding procedure Grab_Events
     (Console : access Gtk_Console_Record; Grab : Boolean);
   overriding procedure Set_As_Default_Console
     (Console     : access Gtk_Console_Record;
      Script      : GNATCOLL.Scripts.Scripting_Language);
   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Gtk_Console_Record);
   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Gtk_Console_Record)
      return Class_Instance;
   overriding procedure Process_Pending_Events_Primitive
     (Console : access Gtk_Console_Record);
   overriding function Read
     (Console    : access Gtk_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String;
   --  See inherited subprograms

   function Create
     (Wraps : access Gtk.Text_View.Gtk_Text_View_Record'Class)
      return Gtk_Console;
   --  Wraps a text_view into a gtk_console. It is recommended that the text
   --  view be put inside a scrolled window.
   --  Pressing <return> in the text view will execute the corresponding
   --  command.

   function Get
     (Console : access Gtk_Console_Record) return Gtk.Text_View.Gtk_Text_View;
   --  Return the widget encapsulated in the console

private
   type Gtk_Console_Record is new Virtual_Console_Record with record
      View     : Gtk.Text_View.Gtk_Text_View;
      Script   : GNATCOLL.Scripts.Scripting_Language;

      Prompt_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The position after which the user can insert text.

      Insert_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The insert position.

      Took_Grab : Boolean := False;

      Waiting_For_Input   : Integer := 0;
      Waiting_For_Newline : Boolean := False;
      --  When the console is blocked in a call to read() or readline(),
      --  the first variable indicates the maximal number of characters to wait
      --  for. The second is true if a newline character should stop the block.

      Destroy_Id : Gtk.Handlers.Handler_Id;
   end record;
end GtkConsole;
