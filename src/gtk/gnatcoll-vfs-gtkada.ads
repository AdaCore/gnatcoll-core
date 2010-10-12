-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                        Copyright (C) 2007-2009, AdaCore           --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- As a special exception, if other files instantiate generics  from --
-- this unit, or you link this  unit with other files to produce  an --
-- executable, this unit does not by itself cause the resulting exe- --
-- cutable  to be covered by  the  GNU General  Public License. This --
-- exception does not however  invalidate any other reasons why  the --
-- executable  file  might  be  covered  by  the  GNU General Public --
-- License.                                                          --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;
with Glib.Values;
with Gtk.Tree_Store;
with Gtk.Tree_Model;

--  This package provides utilities to encapsulate a virtual file
--  in a GValue.

package GNATCOLL.VFS.GtkAda is

   -------------
   -- Gvalues --
   -------------

   procedure Set_File (Value : in out Glib.Values.GValue; File : Virtual_File);
   --  Store File into Value
   --  Value must have been initialized (See Glib.Values.Init) with type
   --  given by Get_Virtual_File_Type, below.

   function Get_File (Value : Glib.Values.GValue) return Virtual_File;
   --  Retrieve the file stored in Value

   function Get_Virtual_File_Type return Glib.GType;
   --  Return the gtype to use for virtual files

   procedure Set_File
     (Tree_Store : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      File       : Virtual_File);
   --  Set a file into a tree store. The column should have been initialized
   --  as GTYpe_Pointer

   function Get_File
     (Tree_Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint) return Virtual_File;
   --  Get a file from a tree model. The column should have been initialized
   --  as GType_Pointer

end GNATCOLL.VFS.GtkAda;
