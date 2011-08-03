-----------------------------------------------------------------------
--                           G N A T C O L L                         --
--                 Copyright (C) 2011, AdaCore                       --
-----------------------------------------------------------------------

with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite;
with Database;

procedure Library is
   Descr : constant Database_Description :=
      GNATCOLL.SQL.Sqlite.Setup ("obj/library.db");
   DB : Database_Connection;
begin
   DB := Descr.Build_Connection;
end Library;
