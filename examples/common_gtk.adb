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

with GNAT.Scripts;               use GNAT.Scripts;
with GNAT.Scripts.Python.Gtkada; use GNAT.Scripts.Python.Gtkada;
with GNAT.Scripts.Gtkada;        use GNAT.Scripts.Gtkada;
with Glib.Object;                use Glib.Object;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Window;                 use Gtk.Window;

package body Common_Gtk is

   procedure Window_Handler
      (Data : in out Callback_Data'Class; Command : String);

   procedure Window_Handler
      (Data : in out Callback_Data'Class; Command : String)
   is
      Inst : Class_Instance := Nth_Arg (Data, 1);
      Win  : Gtk_Window;
   begin
      if Command = Constructor_Method then
         Gtk_New (Win, Window_Toplevel);
         Show_All (Win);
         Set_Data (Inst, GObject (Win));

      elsif Command = "set_title" then
         Win := Gtk_Window (GObject'(Get_Data (Inst)));
         Set_Title (Win, Nth_Arg (Data, 2));
      end if;
   end Window_Handler;

   -------------------------
   -- Add_GUI_Subprograms --
   -------------------------

   procedure Add_GUI_Subprograms (Repo : Scripts_Repository) is
      Py  : Scripting_Language;
      Win : Class_Type;
   begin

      Py  := Lookup_Scripting_Language (Repo, "python");

      --  Example of use:
      --     start newclassgtk
      --  Then enter the following python commands:
      --     w = Hello.Window ()         # Create a new window
      --     w.set_title ("bar")         # Change its title
      --     import gtk                  # Import pygtk
      --     b = gtk.Button("press me")  # Create a new button
      --     w.pywidget().add (b)        # Add it to the window (through pygtk)
      --     b.show_all()                # Make the button visible

      Win := New_Class (Repo, "Window");
      Register_Command
        (Repo, Constructor_Method,
         Class => Win,
         Handler => Window_Handler'Access);
      Register_Command
        (Repo, "set_title", 1, 1,
         Class => Win,
         Handler => Window_Handler'Access);

      Init_PyGtk_Support (Py);
      Add_PyWidget_Method (Py, Win);
   end Add_GUI_Subprograms;

end Common_Gtk;
