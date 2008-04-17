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

with Common;              use Common;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with Gtk.Box;             use Gtk.Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;            use Gtk.Main;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Window;          use Gtk.Window;
with GtkConsole;          use GtkConsole;
with Common_Gtk;          use Common_Gtk;

procedure NewClassGtk is
   Repo     : constant Scripts_Repository := Register_Scripts_And_Functions;
   Console  : Gtk_Console;
   Win      : Gtk_Window;
   Scrolled : Gtk_Scrolled_Window;
   View     : Gtk_Text_View;
   Box      : Gtk_Box;
   Label    : Gtk_Label;
begin
   Add_GUI_Subprograms (Repo);

   Gtk.Main.Init;
   Gtk_New (Win, Window_Toplevel);
   Set_Default_Size (Win, 800, 600);

   Gtk_New_Vbox (Box, Homogeneous => False);
   Add (Win, Box);

   --  For each known language, create a console
   declare
      Langs : constant Scripting_Language_Array :=
        Get_Scripting_Languages (Repo);
   begin
      for L in Langs'Range loop
         Gtk_New (Label, Get_Name (Langs (L)));
         Pack_Start (Box, Label, Expand => False, Fill => False);

         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Box, Scrolled, Expand => True, Fill => True);

         Gtk_New (View);
         Add (Scrolled, View);
         Set_Wrap_Mode (View, Wrap_Word);

         Console := GtkConsole.Create (View);
         Set_Default_Console (Langs (L), Virtual_Console (Console));
      end loop;
   end;

   Show_All (Win);
   Gtk.Main.Main;
end NewClassGtk;
