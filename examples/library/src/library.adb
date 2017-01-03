------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
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

with GNATCOLL.SQL;          use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;     use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.SQL.Inspect;  use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with Database;              use Database;
with ORM;                   use ORM;
with Ada.Text_IO;           use Ada.Text_IO;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

procedure Library is
   Descr : Database_Description :=
      GNATCOLL.SQL.Sqlite.Setup ("obj/library.db");
   DB : Database_Connection;
   Q  : SQL_Query;
   R  : Forward_Cursor;
   Stmt : Prepared_Statement;
begin
   GNATCOLL.Traces.Parse_Config_File (".gnatdebug");  --  show traces

   DB := Descr.Build_Connection;

   --  Load the fixture file
   --  Since we are using "&" references, we need to load the schema too
   Load_Data
      (DB, Create ("fixture.txt"),
       Schema => New_Schema_IO (Create ("dbschema.txt")).Read_Schema);
   DB.Commit;

   ---------------------------------------------------
   --  Part 1: using GNATCOLL.SQL
   --  In this part, we are still manipulating SQL queries ourselves
   ---------------------------------------------------

   --  Get all the books borrowed by the customer "Smith"

   Q := SQL_Select
      (Fields => Books.Title & Books.Pages,
       From   => Books & Customers,
       Where  => Books.FK (Customers) and Customers.Last = "Smith");
   R.Fetch (DB, Q);
   while R.Has_Row loop
      Put_Line ("Borrowed by smith: " & R.Value (0)
                & " pages=" & R.Integer_Value (1)'Img);
      R.Next;
   end loop;

   --  Same as above, using a prepared query. If we had lots of similar
   --  queries to do, this would be much more efficient. The preparation
   --  could also be done in a global variable at elaboration time.

   Q := SQL_Select
      (Fields => Books.Title & Books.Pages,
       From   => Books & Customers,
       Where  => Books.FK (Customers) and Customers.Last = Text_Param (1));
   Stmt := Prepare (Q, On_Server => True, Name => "books_borrowed_by");

   declare
      Smith : aliased String := "Smith";
   begin
      R.Fetch (DB, Stmt, Params => (1 => +Smith'Access));
      while R.Has_Row loop
         Put_Line ("Borrowed by smith: " & R.Value (0)
                   & " pages=" & R.Integer_Value (1)'Img);
         R.Next;
      end loop;
   end;

   --  Free memory for part 1

   R := No_Element;
   Free (DB);
   Free (Descr);

   ---------------------------------------------------
   --  Part 2: using GNATCOLL.SQL.ORM
   --  The following code does the same queries as above, but using the
   --  ORM.
   ---------------------------------------------------

   GNATCOLL.SQL.Sessions.Setup
      (Descr        => GNATCOLL.SQL.Sqlite.Setup ("obj/library.db"),
       Max_Sessions => 2);

   declare
      Session : constant Session_Type := Get_New_Session;
      B : Book_List;
      CL : Customer_List;
      C : ORM.Detached_Customer'Class := Get_Customer (Session, Id => 1);
   begin
      Put_Line ("Customer id 1 is " & C.Last);

      --  SQL-like query
      B := All_Books.Filter
         (Books.FK (Customers) and Customers.Last = "Smith")
         .Select_Related (1, Follow_Left_Join => True)
         .Get (Session);
      while B.Has_Row loop
         Put_Line ("Borrowed by " & B.Element.Borrowed_By.Last
                   & " title=" & B.Element.Title
                   & " pages=" & B.Element.Pages'Img);
         B.Next;
      end loop;

      --  Using the more powerful ORM
      B := C.Borrowed_Books.Get (Session);
      while B.Has_Row loop
         Put_Line ("Borrowed by smith: " & B.Element.Title);
         B.Next;
      end loop;

      --  Change a customer, but do not commit to the database yet. So
      --  the change only exists in memory
      C.Set_Last ("Smit");
      C.Set_First ("Andrew");

      --  Retrieve the list of all customers, and make sure the modified
      --  one is indeed seen with the new values, even though the DBMS
      --  doesn't know about the changes.

      CL := All_Customers.Get (Session);
      while CL.Has_Row loop
         Put_Line ("Customer: " & CL.Element.First & " "
                   & CL.Element.Last);
         CL.Next;
      end loop;

      Session.Commit;
   end;

   GNATCOLL.SQL.Sessions.Free;

   ---------------------------------------------------
   --  Free memory
   ---------------------------------------------------

   GNATCOLL.Traces.Finalize;
end Library;
