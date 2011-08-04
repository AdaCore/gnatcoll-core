-----------------------------------------------------------------------
--                           G N A T C O L L                         --
--                 Copyright (C) 2011, AdaCore                       --
-----------------------------------------------------------------------

with GNATCOLL.SQL;         use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;    use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with Database;             use Database;
with Ada.Text_IO;          use Ada.Text_IO;
with GNATCOLL.Traces;      use GNATCOLL.Traces;

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

   --  Free memory

   Free (DB);
   Free (Descr);
   GNATCOLL.Traces.Finalize;
end Library;
