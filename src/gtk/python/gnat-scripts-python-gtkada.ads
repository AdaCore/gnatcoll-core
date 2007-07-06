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

--  Addition subprograms to support python scripting in graphical mode.
--  In particular, these provide additional support for pygtk

with Glib.Object;

package GNAT.Scripts.Python.Gtkada is

   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class);
   --  Initialize the support for pygtk.
   --  If this fails, the calls to the subprograms below will have no effect.
   --  This suprogram only succeeds when the scripts package was compiled with
   --  pygtk, and the latter is found in the python installation when the
   --  application is run.

   procedure Add_PyWidget_Method
     (Script : access Scripting_Language_Record'Class;
      Class  : Class_Type);
   --  Adds a new method to Class:
   --     Class.pywidget
   --         Returns the pywidget corresponding to the GtkAda widget stored
   --         in Class. It is assumed that Scripts.Gtkada.Get_Data will
   --         return a valid GObject (or null) when called on this instance

   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Glib.Object.GObject;
   --  Return the Gtk object encapsulated inside the pygtk object given in the
   --  N-th argument

end GNAT.Scripts.Python.Gtkada;
