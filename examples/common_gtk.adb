------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python.Gtkada; use GNATCOLL.Scripts.Python.Gtkada;
with GNATCOLL.Scripts.Gtkada;        use GNATCOLL.Scripts.Gtkada;
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
      Add_PyWidget_Method (Repo, Win);
   end Add_GUI_Subprograms;

end Common_Gtk;
