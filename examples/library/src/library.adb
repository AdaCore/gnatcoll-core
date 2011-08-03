-----------------------------------------------------------------------
--                           G N A T C O L L                         --
--                 Copyright (C) 2011, AdaCore                       --
-----------------------------------------------------------------------

with GNATCOLL.SQL.Exec;    use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with Database;

procedure Library is
   Descr : constant Database_Description :=
      GNATCOLL.SQL.Sqlite.Setup ("obj/library.db");
   DB : Database_Connection;
begin
   DB := Descr.Build_Connection;

   --  Load the fixture file
   --  Since we are using "&" references, we need to load the schema too
   Load_Data
      (DB, Create ("fixture.txt"),
       Schema => New_Schema_IO (Create ("dbschema.txt")).Read_Schema);
   DB.Commit;
end Library;
