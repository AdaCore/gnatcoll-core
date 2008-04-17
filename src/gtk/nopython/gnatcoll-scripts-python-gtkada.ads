-----------------------------------------------------------------------
--                               G N A T C O L L                     --
--                                                                   --
--                      Copyright (C) 2003-2008, AdaCore             --
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

with Glib.Object;

package GNATCOLL.Scripts.Python.Gtkada is
   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class);
   procedure Add_PyWidget_Method
     (Script : access Scripting_Language_Record'Class;
      Class  : Class_Type);
   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Glib.Object.GObject;
   --  Does nothing, since python support was not compiled in

end GNATCOLL.Scripts.Python.Gtkada;
