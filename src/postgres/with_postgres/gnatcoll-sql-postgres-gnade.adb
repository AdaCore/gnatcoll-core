-------------------------------------------------------------------------------
--                                                                           --
--                      GNADE  : GNu Ada Database Environment                --
--                                                                           --
--  Author          : Juergen Pfeifer <juergen.pfeifer@gmx.net>
--  Created On      : 29-Oct-2000
--  Last Modified By: $Author$
--  Last Modified On: $Date$
--  Status          : $State$
--
--  Copyright (C) 2000-2003 Juergen Pfeifer
--  Copyright (C) 2004-2015, AdaCore                                         --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  GNADE is implemented to work with GNAT, the GNU Ada compiler.            --
--                                                                           --
--  This binding is not intended to provide a Layer that hides the details   --
--  of PostgreSQL, instead the opposite is intended. This binding exposes    --
--  the same functionality like the C interface libpq.                       --
--  Currently we do not support the asynchronous command features of         --
--  PostgreSQL, but we hope we'll have a nice Ada95 integration of that in   --
--  the near future.                                                         --
-------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;
with Interfaces.C;    use Interfaces.C;
with System;          use System;

package body GNATCOLL.SQL.Postgres.Gnade is
   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   subtype char_array is C.char_array;
   subtype chars_ptr  is CS.chars_ptr;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Database) is
      procedure PQfinish (Connection : PGconnection);
      pragma Import (C, PQfinish, "PQfinish");
   begin
      if Object.Connection /= Null_Connection then
         PQfinish (Object.Connection);
         Object.Connection := Null_Connection;
      end if;
   end Finalize;

   -------------
   -- Connect --
   -------------

   function Connect (Params : access String) return PGconnection is
      function PQConnect (Options :  char_array) return PGconnection;
      pragma Import (C, PQConnect, "PQconnectdb");
      Conn : PGconnection;
   begin
      Conn := PQConnect (C.To_C (Params.all));
      if Conn = Null_Connection then
         raise PostgreSQL_Error;
      else
         return Conn;
      end if;
   end Connect;

   -----------
   -- Reset --
   -----------

   procedure Reset (DB : Database'Class) is
      procedure PQreset (Connection : PGconnection);
      pragma Import (C, PQreset, "PQreset");
   begin
      PQreset (DB.Connection);
   end Reset;

   -------------------------
   -- Connection_Accessor --
   -------------------------

   generic
      with function Accessor (Conn : PGconnection) return chars_ptr;
   function Connection_Accessor (DB : Database'Class) return String;

   function Connection_Accessor (DB : Database'Class) return String is
   begin
      return CS.Value (Accessor (DB.Connection));
   end Connection_Accessor;

   function PQdb (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQdb, "PQdb");
   function Get_Name is new Connection_Accessor (Accessor => PQdb);

   function PQuser (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQuser, "PQuser");
   function Get_User is new Connection_Accessor (Accessor => PQuser);

   function PQpass (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQpass, "PQpass");
   function Get_Pass is new Connection_Accessor (Accessor => PQpass);

   function PQhost (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQhost, "PQhost");
   function Get_Host is new Connection_Accessor (Accessor => PQhost);

   function PQport (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQport, "PQport");
   function Get_Port is new Connection_Accessor (Accessor => PQport);

   function PQtty (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQtty, "PQtty");
   function Get_TTY is new Connection_Accessor (Accessor => PQtty);

   function PQopt (Conn : PGconnection) return chars_ptr;
   pragma Import (C, PQopt, "PQoptions");
   function Get_Options is new Connection_Accessor (Accessor => PQopt);

   function Name     (DB : Database'Class) return String renames Get_Name;
   function User     (DB : Database'Class) return String renames Get_User;
   function Password (DB : Database'Class) return String renames Get_Pass;
   function Host     (DB : Database'Class) return String renames Get_Host;
   function Port     (DB : Database'Class) return String renames Get_Port;
   function TTY      (DB : Database'Class) return String renames Get_TTY;
   function Options  (DB : Database'Class) return String
     renames Get_Options;

   -----------
   -- Error --
   -----------

   function Error (DB : Database'Class) return String is
      function PQerr (Conn : PGconnection) return chars_ptr;
      pragma Import (C, PQerr, "PQerrorMessage");
   begin
      return CS.Value (PQerr (DB.Connection));
   end Error;

   ------------
   -- Status --
   ------------

   function Status (DB : Database'Class) return ConnStatus is
      function PQstatus (Conn : PGconnection) return Interfaces.C.int;
      pragma Import (C, PQstatus, "PQstatus");
   begin
      return ConnStatus'Val (PQstatus (DB.Connection));
   end Status;

   ----------------
   -- Server_PID --
   ----------------

   function Server_PID (DB : Database'Class) return Backend_PID is
      function PQpid (Conn : PGconnection) return Interfaces.C.int;
      pragma Import (C, PQpid, "PQbackendPID");
      function To_PID is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.int, Target => Backend_PID);
   begin
      return To_PID (PQpid (DB.Connection));
   end Server_PID;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Res    : in out Result;
      DB     : Database'Class;
      Query  : String;
      Format : GNATCOLL.SQL_Impl.Formatter'Class;
      Params : SQL_Parameters := No_Parameters)
   is
      function PQexec (Conn : PGconnection; Qry  : chars_ptr) return PGresult;
      pragma Import (C, PQexec, "PQexec");

      function PQexecParams
        (Conn         : PGconnection;
         Command      : chars_ptr;
         nParams      : Interfaces.C.int;
         paramTypes   : System.Address := System.Null_Address;  --  Oid*
         paramValues  : CS.chars_ptr_array;  --  const char* const *
         paramLengths : System.Address := System.Null_Address;  --  int*
         paramFormats : System.Address := System.Null_Address;  --  int*
         resultFormat : Interfaces.C.int := 0) return PGresult;
      pragma Import (C, PQexecParams, "PQexecParams");
      --  paramTypes can be left to NULL for automatic guessing
      --
      --  paramValues:
      --  Specifies the actual values of the parameters. A null pointer in
      --  this array means the corresponding parameter is null; otherwise
      --  the pointer points to a zero-terminated text string (for text
      --  format) or binary data in the format expected by the server (for
      --  binary format).
      --
      --  paramLengths:
      --  Specifies the actual data lengths of binary-format parameters. It
      --  is ignored for null parameters and text-format parameters. The
      --  array pointer can be null when there are no binary parameters.
      --
      --  paramFormats:
      --  Specifies whether parameters are text (put a zero in the array
      --  entry for the corresponding parameter) or binary (put a one in the
      --  array entry for the corresponding parameter). If the array pointer
      --  is null then all parameters are presumed to be text strings.
      --  Values passed in binary format require knowledge of the internal
      --  representation expected by the backend. For example, integers must
      --  be passed in network byte order. Passing numeric values requires
      --  knowledge of the server storage format.
      --
      --  resultFormat
      --  Specify zero to obtain results in text format, or one to obtain
      --  results in binary format. (There is not currently a provision to
      --  obtain different result columns in different formats, although
      --  that is possible in the underlying protocol.)

      P : chars_ptr := CS.New_String (Query);
      R : PGresult;

   begin
      if Params'Length = 0 then
         R := PQexec (DB.Connection, P);
      else
         declare
            Vals : CS.chars_ptr_array (0 .. Params'Length - 1);
         begin
            for P in Params'Range loop
               --  Special case for strings, to avoid using the stack
               if Params (P).Typ = Parameter_Text then
                  Vals (size_t (P - Params'First)) :=
                    CS.New_String (Params (P).Str_Val.all);
               else
                  Vals (size_t (P - Params'First)) :=
                    CS.New_String (Image (Format, Params (P)));
               end if;
            end loop;

            R := PQexecParams
              (Conn    => DB.Connection,
               Command => P,
               nParams => C.int (Vals'Length),
               paramValues => Vals);

            for P in Vals'Range loop
               CS.Free (Vals (P));
            end loop;
         end;
      end if;

      CS.Free (P);
      Clear (Res); --  Free previous results
      Res.Res := R;
   end Execute;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (Res       : out Result;
      DB        : Database'Class;
      Stmt_Name : String;
      Query     : String)
   is
      function PQprepare
        (Conn    : PGconnection;
         Name    : String;
         Query   : String;
         nParams : Natural := 0;
         Types   : System.Address := System.Null_Address)
         return PGresult;
      pragma Import (C, PQprepare, "gnatcoll_pqprepare");

   begin
      Clear (Res); --  Free previous results
      Res.Res :=
        PQprepare (DB.Connection, Stmt_Name & ASCII.NUL, Query & ASCII.NUL);
   end Prepare;

   -------------------
   -- Exec_Prepared --
   -------------------

   procedure Exec_Prepared
     (Res       : out Result;
      DB        : Database'Class;
      Stmt_Name : String;
      Format    : GNATCOLL.SQL_Impl.Formatter'Class;
      Params    : SQL_Parameters := No_Parameters)
   is
      function PQexecPrepared
        (Conn    : PGconnection;
         Name    : String;
         Nparams : Natural := 0;
         Values  : System.Address := System.Null_Address;
         Lengths : System.Address := System.Null_Address;
         Formats : System.Address := System.Null_Address;
         Format  : Natural := 0) return PGresult;
      pragma Import (C, PQexecPrepared, "PQexecPrepared");

      R : PGresult;

   begin
      Clear (Res); --  Free previous results

      if Params'Length = 0 then
         R := PQexecPrepared (DB.Connection, Stmt_Name & ASCII.NUL);

      else
         declare
            Vals : aliased CS.chars_ptr_array (0 .. Params'Length - 1);
         begin
            for P in Vals'Range loop
               case Params (Integer (P) + Params'First).Typ is
                  when Parameter_Text =>
                     --  Special case for text, which is already well formated
                     Vals (P) := CS.New_String
                       (Params (Integer (P) + Params'First).Str_Val.all);

                  when others =>
                     Vals (P) := CS.New_String
                       (Image (Format, Params (Integer (P) + Params'First)));
               end case;
            end loop;

            R := PQexecPrepared
              (DB.Connection,
               Stmt_Name & ASCII.NUL,
               Nparams => Vals'Length,
               Values  => Vals (0)'Address);

            for P in Vals'Range loop
               CS.Free (Vals (P));
            end loop;
         end;
      end if;

      Res.Res := R;
   end Exec_Prepared;

   -----------------
   -- BLOB_Create --
   -----------------

   function BLOB_Create
     (DB : Database'Class; Mode : File_Mode) return OID
   is
      function LO_Creat (Conn : PGconnection; Mode : C.int) return OID;
      pragma Import (C, LO_Creat, "lo_creat");
   begin
      return LO_Creat (DB.Connection, C.int (Mode));
   end BLOB_Create;

   -----------------
   -- BLOB_Import --
   -----------------

   function BLOB_Import (DB           : Database'Class;
                         In_File_Name : String) return OID
   is
      function LO_Import (Conn : PGconnection; File : chars_ptr) return OID;
      pragma Import (C, LO_Import, "lo_import");
      P : chars_ptr := CS.New_String (In_File_Name);
      Obj_Id : constant OID := LO_Import (DB.Connection, P);
   begin
      CS.Free (P);
      return Obj_Id;
   end BLOB_Import;

   -----------------
   -- BLOB_Export --
   -----------------

   function BLOB_Export (DB            : Database'Class;
                         Object_Id     : OID;
                         Out_File_Name : String) return Boolean
   is
      function LO_Export (Conn   : PGconnection;
                          Obj_Id : OID;
                          File   : chars_ptr) return C.int;
      pragma Import (C, LO_Export, "lo_export");
      P : chars_ptr := CS.New_String (Out_File_Name);
      B : constant Boolean := LO_Export (DB.Connection, Object_Id, P) >= 0;
   begin
      CS.Free (P);
      return B;
   end BLOB_Export;

   ---------------
   -- BLOB_Open --
   ---------------

   function BLOB_Open (DB        : Database'Class;
                       Object_Id : OID;
                       Mode      : File_Mode)
                       return File_Descriptor
   is
      function LO_Open (Conn   : PGconnection;
                        Obj_Id : OID;
                        Mode   : C.int) return Integer;
      pragma Import (C, LO_Open, "lo_open");
   begin
      return File_Descriptor
        (LO_Open (DB.Connection, Object_Id, C.int (Mode)));
   end BLOB_Open;

   ----------------
   -- BLOB_Write --
   ----------------

   function BLOB_Write (DB : Database'Class;
                        FD : File_Descriptor;
                        A  : System.Address;
                        N  : Integer)
                        return Integer
   is
      function LO_Write (Conn : PGconnection;
                         FD   : C.int;
                         A    : System.Address;
                         N    : C.int) return Integer;
      pragma Import (C, LO_Write, "lo_write");
   begin
      return LO_Write (DB.Connection, C.int (FD), A, C.int (N));
   end BLOB_Write;

   ---------------
   -- BLOB_Read --
   ---------------

   function BLOB_Read (DB : Database'Class;
                       FD : File_Descriptor;
                       A  : System.Address;
                       N  : Integer)
                       return Integer
   is
      function LO_Read (Conn : PGconnection;
                        FD   : C.int;
                        A    : System.Address;
                        N    : C.int) return Integer;
      pragma Import (C, LO_Read, "lo_read");
   begin
      return LO_Read (DB.Connection, C.int (FD), A, C.int (N));
   end BLOB_Read;

   ----------------
   -- BLOB_Lseek --
   ----------------

   function BLOB_Lseek (DB     : Database'Class;
                        FD     : File_Descriptor;
                        Offset : Integer;
                        Origin : Integer)
                        return Integer
   is
      function LO_Lseek (Conn   : PGconnection;
                         FD     : C.int;
                         Offset : C.int;
                         Origin : C.int) return C.int;
      pragma Import (C, LO_Lseek, "lo_lseek");
   begin
      return Integer
        (LO_Lseek (DB.Connection, C.int (FD), C.int (Offset), C.int (Origin)));
   end BLOB_Lseek;

   ---------------
   -- BLOB_Tell --
   ---------------

   function BLOB_Tell (DB : Database'Class;
                       FD : File_Descriptor)
                       return Integer
   is
      function LO_Tell (Conn : PGconnection; FD   : C.int) return C.int;
      pragma Import (C, LO_Tell, "lo_tell");
   begin
      return Integer (LO_Tell (DB.Connection, C.int (FD)));
   end BLOB_Tell;

   ----------------
   -- BLOB_Close --
   ----------------

   function BLOB_Close (DB : Database'Class;
                        FD : File_Descriptor)
                        return Boolean
   is
      function LO_Close (Conn : PGconnection; FD   : C.int) return C.int;
      pragma Import (C, LO_Close, "lo_close");
   begin
      return LO_Close (DB.Connection, C.int (FD)) >= 0;
   end BLOB_Close;

   -----------------
   -- BLOB_Unlink --
   -----------------

   function BLOB_Unlink (DB        : Database'Class;
                         Object_Id : OID) return Boolean
   is
      function LO_Unlink (Conn : PGconnection; Object_Id : OID) return C.int;
      pragma Import (C, LO_Unlink, "lo_unlink");
   begin
      return LO_Unlink (DB.Connection, Object_Id) >= 0;
   end BLOB_Unlink;

   -----------------------
   -- Make_Empty_Result --
   -----------------------

   procedure Make_Empty_Result
     (Res    : out Result;
      DB     : Database'Class;
      Status : ExecStatus := PGRES_EMPTY_QUERY)
   is
      function PQemptyRes (D : PGconnection; Status : C.int) return PGresult;
      pragma Import (C, PQemptyRes, "PQmakeEmptyPGresult");
      R : constant PGresult :=
        PQemptyRes (DB.Connection, ExecStatus'Pos (Status));
   begin
      if R = Null_Result then
         raise PostgreSQL_Error;
      end if;

      Res.Res := R;
   end Make_Empty_Result;

   -----------
   -- Clear --
   -----------

   procedure Clear (Res : in out Result) is
      procedure PQclear (Res : PGresult);
      pragma Import (C, PQclear, "PQclear");
   begin
      if Res.Res /= Null_Result then
         PQclear (Res.Res);
         Res.Res := Null_Result;
      end if;
   end Clear;

   ------------
   -- Status --
   ------------

   function Status (Res : Result) return ExecStatus is
      function PQresStatus (Res : PGresult) return Interfaces.C.int;
      pragma Import (C, PQresStatus, "PQresultStatus");
   begin
      if Res.Res = Null_Result then
         return PGRES_Null_Result;
      else
         return ExecStatus'Val (PQresStatus (Res.Res));
      end if;
   end Status;

   ------------
   -- Status --
   ------------

   function Status (Status : ExecStatus) return String is
      function PQresStat (stat : Interfaces.C.int) return chars_ptr;
      pragma Import (C, PQresStat, "PQresStatus");
   begin
      if Status = PGRES_Null_Result then
         return "<null result>";
      else
         return CS.Value (PQresStat (ExecStatus'Pos (Status)));
      end if;
   end Status;

   ------------
   -- Status --
   ------------

   function Status (Res : Result) return String is
      Stat :  constant ExecStatus := Status (Res);
   begin
      return Status (Stat);
   end Status;

   -----------
   -- Error --
   -----------

   function Error (Res : Result) return String is
      function PQresErr (Res : PGresult) return chars_ptr;
      pragma Import (C, PQresErr, "PQresultErrorMessage");
   begin
      return CS.Value (PQresErr (Res.Res));
   end Error;

   function Quote_Identifier (Identifier : String) return String is
   begin
      return '"' & Identifier & '"';
   end Quote_Identifier;

   -------------------
   -- Info_Accessor --
   -------------------

   generic
      with function Accessor (Res : PGresult) return C.int;
   function Info_Accessor (Res : Result) return Integer;

   function Info_Accessor (Res : Result) return Integer is
   begin
      return Integer (Accessor (Res.Res));
   end Info_Accessor;

   function PQntuples (Res : PGresult) return C.int;
   pragma Import (C, PQntuples, "PQntuples");
   function Get_Count is new Info_Accessor (Accessor => PQntuples);

   function PQnfields (Res : PGresult) return C.int;
   pragma Import (C, PQnfields, "PQnfields");
   function Get_FCount is new Info_Accessor (Accessor => PQnfields);

   function PQbinaryTuples (Res : PGresult) return C.int;
   pragma Import (C, PQbinaryTuples, "PQbinaryTuples");
   function Get_BinaryTuples is new Info_Accessor (Accessor => PQbinaryTuples);

   -----------------
   -- Tuple_Count --
   -----------------

   function Tuple_Count (Res : Result) return Tuple_Index is
   begin
      return Tuple_Index (Get_Count (Res));
   end Tuple_Count;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Res : Result) return Field_Index is
   begin
      return Field_Index (Get_FCount (Res));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name  (Res   : Result; Index : Field_Index) return String is
      function PQfname (Res : PGresult; Idx : C.int) return chars_ptr;
      pragma Import (C, PQfname, "PQfname");
   begin
      return CS.Value (PQfname (Res.Res, C.int (Index)));
   end Field_Name;

   ------------------
   -- Field_Lookup --
   ------------------

   procedure Field_Lookup (Res   : Result;
                           Name  : String;
                           Index : out Field_Index;
                           Found : out Boolean)
   is
      function PQfnumber (Res  : PGresult; Name : chars_ptr) return C.int;
      pragma Import (C, PQfnumber, "PQfnumber");
      P : chars_ptr := CS.New_String (Name);
      I : constant C.int := PQfnumber (Res.Res, P);
   begin
      CS.Free (P);
      if I < 0 then
         Found := False;
         Index := Field_Index'Last;
      else
         Found := True;
         Index := Field_Index (I);
      end if;
   end Field_Lookup;

   ---------------
   -- Is_Binary --
   ---------------

   function Is_Binary (Res : Result) return Boolean is
   begin
      return Get_BinaryTuples (Res) /= 0;
   end Is_Binary;

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type
     (Res : Result; Index : Field_Index) return TypeID
   is
      function PQftype (Res : PGresult; Idx : C.int) return TypeID;
      pragma Import (C, PQftype, "PQftype");
   begin
      return PQftype (Res.Res, C.int (Index));
   end Field_Type;

   -----------
   -- Value --
   -----------

   procedure Value (Res     : Result;
                    Tuple   : Tuple_Index;
                    Field   : Field_Index;
                    Pointer : out System.Address)
   is
      function Cvt is new Ada.Unchecked_Conversion (chars_ptr, System.Address);
      function PQgetvalue (Res   : PGresult;
                           Tuple : C.int;
                           Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
      P : constant chars_ptr :=
        PQgetvalue (Res.Res, C.int (Tuple), C.int (Field));
   begin
      Pointer := Cvt (P);
   end Value;

   -------------
   -- C_Value --
   -------------

   function C_Value
     (Res   : Result;
      Tuple : Tuple_Index := 0;
      Field : Field_Index := 0) return Interfaces.C.Strings.chars_ptr
   is
      function PQgetvalue (Res   : PGresult;
                           Tuple : C.int;
                           Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
   begin
      return PQgetvalue (Res.Res, C.int (Tuple), C.int (Field));
   end C_Value;

   -----------
   -- Value --
   -----------

   function Value (Res   : Result;
                   Tuple : Tuple_Index := 0;
                   Field : Field_Index := 0) return String
   is
      function PQgetvalue
        (Res   : PGresult; Tuple, Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
      P : constant chars_ptr :=
        PQgetvalue (Res.Res, C.int (Tuple), C.int (Field));
   begin
      if Is_Binary (Res) then
         raise PostgreSQL_Error;
      end if;
      return CS.Value (P);
   end Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Res   : Result;
                           Tuple : Tuple_Index := 0;
                           Field : Field_Index := 0) return Boolean is
   begin
      return Value (Res, Tuple, Field) = "t";
   end Boolean_Value;

   -----------
   -- Value --
   -----------

   function Value (Res        : Result;
                   Tuple      : Tuple_Index := 0;
                   Field_Name : String) return String
   is
      Found : Boolean;
      Idx   : Field_Index;
   begin
      Field_Lookup (Res, Field_Name, Idx, Found);
      if Found then
         return Value (Res, Tuple, Idx);
      else
         raise PostgreSQL_Error;
      end if;
   end Value;

   -------------------
   -- Boolean_Value --
   -------------------

   function Boolean_Value (Res        : Result;
                           Tuple      : Tuple_Index := 0;
                           Field_Name : String) return Boolean is
   begin
      return Value (Res, Tuple, Field_Name) = "t";
   end Boolean_Value;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Res     : Result;
                           Tuple   : Tuple_Index := 0;
                           Field   : Field_Index := 0;
                           Default : Integer := Integer'First) return Integer
   is
   begin
      return Integer'Value (Value (Res, Tuple, Field));
   exception
      when Constraint_Error =>
         if Default = Integer'First then
            raise PostgreSQL_Error;
         else
            return Default;
         end if;
   end Integer_Value;

   ----------------
   -- Field_Size --
   ----------------

   function Field_Size
     (Res   : Result; Field : Field_Index) return Integer
   is
      function PQfsize (Res : PGresult; Idx : C.int) return C.int;
      pragma Import (C, PQfsize, "PQfsize");
   begin
      return Integer (PQfsize (Res.Res, C.int (Field)));
   end Field_Size;

   ------------------------
   -- Field_Modification --
   ------------------------

   function Field_Modification
     (Res   : Result; Field : Field_Index) return Integer
   is
      function PQfmod (Res : PGresult; Idx : C.int) return C.int;
      pragma Import (C, PQfmod, "PQfmod");
   begin
      return Integer (PQfmod (Res.Res, C.int (Field)));
   end Field_Modification;

   ------------------
   -- Field_Length --
   ------------------

   function Field_Length
     (Res : Result; Tuple : Tuple_Index; Field : Field_Index) return Natural
   is
      function PQgetlen (Res : PGresult; Row, Idx : C.int) return C.int;
      pragma Import (C, PQgetlen, "PQgetlength");
   begin
      return Natural (PQgetlen (Res.Res, C.int (Tuple), C.int (Field)));
   end Field_Length;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Res   : Result;
      Tuple : Tuple_Index;
      Field : Field_Index) return Boolean
   is
      function PQisnull (Res : PGresult; Row, Idx : C.int) return C.int;
      pragma Import (C, PQisnull, "PQgetisnull");
   begin
      return PQisnull (Res.Res, C.int (Tuple), C.int (Field)) /= 0;
   end Is_Null;

   --------------------
   -- Command_Status --
   --------------------

   function Command_Status (Res : Result) return String is
      function PQcmdStatus (Res : PGresult) return chars_ptr;
      pragma Import (C, PQcmdStatus, "PQcmdStatus");
   begin
      return CS.Value (PQcmdStatus (Res.Res));
   end Command_Status;

   --------------------
   -- Command_Tuples --
   --------------------

   function Command_Tuples (Res : Result) return String is
      function PQcmdTuples (Res : PGresult) return chars_ptr;
      pragma Import (C, PQcmdTuples, "PQcmdTuples");
   begin
      return CS.Value (PQcmdTuples (Res.Res));
   end Command_Tuples;

   --------------------
   -- Command_Tuples --
   --------------------

   function Command_Tuples (Res : Result) return Natural is
      S : constant String := Command_Tuples (Res);
   begin
      if S = "" then
         return 0;
      else
         return Natural'Value (S);
      end if;
   end Command_Tuples;

   ---------------
   -- OID_Value --
   ---------------

   function OID_Value (Res : Result) return OID is
      function PQoidValue (Res : PGresult) return OID;
      pragma Import (C, PQoidValue, "PQoidValue");
   begin
      return PQoidValue (Res.Res);
   end OID_Value;

   ---------------------
   -- Is_Non_Blocking --
   ---------------------

   function Is_Non_Blocking (DB : Database'Class) return Boolean is
      function PQisnonblocking (Conn : PGconnection) return C.int;
      pragma Import (C, PQisnonblocking, "PQisnonblocking");
   begin
      return PQisnonblocking (DB.Connection) /= 0;
   end Is_Non_Blocking;

   ----------------
   -- Send_Query --
   ----------------

   function Send_Query
     (DB    : Database'Class; Query : String) return Boolean
   is
      function PQsendQuery (Conn : PGconnection; Qry : String) return C.int;
      pragma Import (C, PQsendQuery, "PQsendQuery");

   begin
      return PQsendQuery (DB.Connection, Query & ASCII.NUL) /= 0;
   end Send_Query;

   ----------------
   -- Get_Result --
   ----------------

   procedure Get_Result (DB   : Database'Class;
                         Res  : out Result;
                         Done : out Boolean)
   is
      function PQgetResult (Conn : PGconnection) return PGresult;
      pragma Import (C, PQgetResult, "PQgetResult");
      R : constant PGresult := PQgetResult (DB.Connection);
   begin
      Res.Res := R;
      Done := R = Null_Result;
   end Get_Result;

   -------------------
   -- Consume_Input --
   -------------------

   function Consume_Input (DB : Database'Class) return Boolean is
      function PQconsumeInput (Conn : PGconnection) return C.int;
      pragma Import (C, PQconsumeInput, "PQconsumeInput");
   begin
      return PQconsumeInput (DB.Connection) /= 0;
   end Consume_Input;

   -----------
   -- Flush --
   -----------

   function Flush (DB : Database'Class) return Boolean is
      function PQflush (Conn : PGconnection) return C.int;
      pragma Import (C, PQflush, "PQflush");
   begin
      return PQflush (DB.Connection) = 0;
   end Flush;

   -------------
   -- Is_Busy --
   -------------

   function Is_Busy (DB : Database'Class) return Boolean is
      function PQisBusy (Conn : PGconnection) return C.int;
      pragma Import (C, PQisBusy, "PQisBusy");
   begin
      return PQisBusy (DB.Connection) /= 0;
   end Is_Busy;

   --------------------
   -- Request_Cancel --
   --------------------

   function Request_Cancel (DB : Database'Class) return Boolean is
      function PQrequestCancel (Conn : PGconnection) return C.int;
      pragma Import (C, PQrequestCancel, "PQrequestCancel");
   begin
      return PQrequestCancel (DB.Connection) /= 0;
   end Request_Cancel;

   ------------
   -- Socket --
   ------------

   function Socket (DB : Database'Class) return Interfaces.C.int is
      function PQsocket (Conn : PGconnection) return C.int;
      pragma Import (C, PQsocket, "PQsocket");
   begin
      return PQsocket (DB.Connection);
   end Socket;

   --------------
   -- Notifies --
   --------------

   procedure Notifies (DB      : Database'Class;
                       Message : out Notification;
                       Done    : out Boolean)
   is
      function PQnotifies (Conn : PGconnection) return System.Address;
      pragma Import (C, PQnotifies, "PQnotifies");

      procedure Free (Addr : System.Address);
      pragma Import (C, Free, "free");

      type NotiPtr is access all Notification;
      package P is new System.Address_To_Access_Conversions (Notification);
      function Cvt is new Ada.Unchecked_Conversion (P.Object_Pointer, NotiPtr);
      N : NotiPtr;
      A : constant System.Address := PQnotifies (DB.Connection);

   begin
      N := Cvt (P.To_Pointer (A));
      if N /= null then
         Message := N.all;
         Free (A);
         Done := False;
      else
         Done := True;
      end if;
   end Notifies;

   -----------------
   -- Array_Field --
   -----------------

   function Array_Field (Value : String; Field : Positive) return String is
      Pos   : Integer := Value'First + 1;
      Last  : Integer;
      Index : Positive := 1;
   begin
      if Value (Value'First) = '{' then
         while Pos <= Value'Last loop
            Last := Pos + 1;

            --  Will raise Constraint_Error in the end
            while Value (Last) /= '}' and then Value (Last) /= ',' loop
               Last := Last + 1;
            end loop;

            if Field = Index then
               return Value (Pos .. Last - 1);
            end if;

            Index := Index + 1;
            Pos := Last + 1;
         end loop;
      end if;

      raise Constraint_Error;
   end Array_Field;

end GNATCOLL.SQL.Postgres.Gnade;
