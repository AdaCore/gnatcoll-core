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
--  Copyright (C) 2004-2011, AdaCore                                         --
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
with Ada.Unchecked_Deallocation;
with Interfaces.C; use Interfaces.C;
with System;

package body GNATCOLL.SQL.Postgres.Gnade is
   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   subtype char_array is C.char_array;
   subtype chars_ptr  is CS.chars_ptr;

   function To_Result is new Ada.Unchecked_Conversion (System.Address,
                                                       PGresult);
   function To_Addr is new Ada.Unchecked_Conversion (PGresult,
                                                     System.Address);

   function To_Conn is new Ada.Unchecked_Conversion (System.Address,
                                                     Connection_Handle);
   function To_Addr is new Ada.Unchecked_Conversion (Connection_Handle,
                                                     System.Address);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (PGConn, Connection_Pointer);

   ------------
   -- PGConn --
   ------------

   protected body PGConn is

      ------------
      -- Handle --
      ------------

      function Handle return Connection_Handle is
      begin
         return C_Connection;
      end Handle;

      -----------
      -- Reset --
      -----------

      procedure Reset is
         procedure PQreset (Connection : System.Address);
         pragma Import (C, PQreset, "PQreset");
      begin
         PQreset (To_Addr (C_Connection));
      end Reset;

      -----------
      -- Error --
      -----------

      function Error return String is
         function PQerr (Conn : System.Address) return chars_ptr;
         pragma Import (C, PQerr, "PQerrorMessage");
      begin
         return CS.Value (PQerr (To_Addr (C_Connection)));
      end Error;

      ------------
      -- Status --
      ------------

      function Status return ConnStatus is
         function PQstatus (Conn : System.Address) return Interfaces.C.int;
         pragma Import (C, PQstatus, "PQstatus");
      begin
         return ConnStatus'Val (PQstatus (To_Addr (C_Connection)));
      end Status;

      -----------
      -- Close --
      -----------

      procedure Close is
         procedure PQfinish (Connection : System.Address);
         pragma Import (C, PQfinish, "PQfinish");
      begin
         if C_Connection /= Null_Connection then
            PQfinish (To_Addr (C_Connection));
            C_Connection := Null_Connection;
         end if;
      end Close;

      ---------
      -- PID --
      ---------

      function PID return Backend_PID is
         function PQpid (Conn : System.Address) return Interfaces.C.int;
         pragma Import (C, PQpid, "PQbackendPID");
         function To_PID is new Ada.Unchecked_Conversion
           (Source => Interfaces.C.int, Target => Backend_PID);
      begin
         return To_PID (PQpid (To_Addr (C_Connection)));
      end PID;

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Res     : in out Result;
         Success : out Boolean;
         Query   : String;
         Format  : GNATCOLL.SQL_Impl.Formatter'Class;
         Params  : SQL_Parameters := No_Parameters)
      is
         function PQexec (Conn : System.Address;
                          Qry  : chars_ptr) return System.Address;
         pragma Import (C, PQexec, "PQexec");

         function PQexecParams
           (Conn         : System.Address;
            Command      : chars_ptr;
            nParams      : Interfaces.C.int;
            paramTypes   : System.Address := System.Null_Address;  --  Oid*
            paramValues  : CS.chars_ptr_array;  --  const char* const *
            paramLengths : System.Address := System.Null_Address;  --  int*
            paramFormats : System.Address := System.Null_Address;  --  int*
            resultFormat : Interfaces.C.int := 0) return System.Address;
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
         Stat : ExecStatus;

      begin
         if Params'Length = 0 then
            R := To_Result (PQexec (To_Addr (C_Connection), P));
         else
            declare
               Vals : CS.chars_ptr_array (0 .. Params'Length - 1);
            begin
               for P in Params'Range loop
                  Vals (size_t (P - Params'First)) :=
                    CS.New_String (Image (Format, Params (P)));
               end loop;

               R := To_Result
                 (PQexecParams
                    (Conn    => To_Addr (C_Connection),
                     Command => P,
                     nParams => C.int (Vals'Length),
                     paramValues => Vals));

               for P in Vals'Range loop
                  CS.Free (Vals (P));
               end loop;
            end;
         end if;

         CS.Free (P);
         Clear (Res); --  Free previous results

         if R = Null_Result then
            Res.Res := Null_Result;
            Success := False;
         else
            Res.Res := R;
            Stat    := Status (Res);
            Success := Stat /= PGRES_FATAL_ERROR;
         end if;
      end Execute;

      -------------
      -- Prepare --
      -------------

      procedure Prepare
        (Res       : out Result;
         Stmt_Name : String;
         Query     : String)
      is
         function PQprepare
           (Conn    : System.Address;
            Name    : String;
            Qry     : String;
            nParams : Natural := 0;
            Types   : System.Address := System.Null_Address)
            return System.Address;
         pragma Import (C, PQprepare, "gnatcoll_pqprepare");

         R : constant PGresult :=
           To_Result
             (PQprepare
                  (To_Addr (C_Connection),
                   Stmt_Name & ASCII.NUL, Query & ASCII.NUL));

      begin
         Clear (Res); --  Free previous results

         if R = Null_Result then
            Res.Res := Null_Result;
         else
            Res.Res := R;
         end if;
      end Prepare;

      -------------------
      -- Exec_Prepared --
      -------------------

      procedure Exec_Prepared
        (Res       : out Result;
         Stmt_Name : String;
         Format    : GNATCOLL.SQL_Impl.Formatter'Class;
         Params    : SQL_Parameters := No_Parameters)
      is
         function PQexecPrepared
           (Conn : System.Address;
            Name : String;
            Nparams : Natural := 0;
            Values  : CS.chars_ptr_array;
            Lengths : System.Address := System.Null_Address;
            Formats : System.Address := System.Null_Address;
            Format  : Natural := 0) return System.Address;
         pragma Import (C, PQexecPrepared, "PQexecPrepared");

         R : PGresult;
         Vals : CS.chars_ptr_array (0 .. Params'Length - 1);

      begin
         for P in Vals'Range loop
            Vals (P) := CS.New_String
              (Image (Format, Params (Integer (P) + Params'First)));
         end loop;

         R := To_Result
           (PQexecPrepared
              (To_Addr (C_Connection),
               Stmt_Name & ASCII.NUL,
               Nparams => Vals'Length,
               Values  => Vals));

         for P in Vals'Range loop
            CS.Free (Vals (P));
         end loop;

         Clear (Res); --  Free previous results

         if R = Null_Result then
            Res.Res := Null_Result;
         else
            Res.Res := R;
         end if;
      end Exec_Prepared;

      -----------------
      -- BLOB_Create --
      -----------------

      function BLOB_Create (Mode : File_Mode) return OID
      is
         function LO_Creat (Conn : System.Address;
                            Mode : C.int) return OID;
         pragma Import (C, LO_Creat, "lo_creat");
      begin
         return LO_Creat (To_Addr (C_Connection), C.int (Mode));
      end BLOB_Create;

      -----------------
      -- BLOB_Import --
      -----------------

      function BLOB_Import (In_File_Name : String) return OID
      is
         function LO_Import (Conn : System.Address;
                             File : chars_ptr) return OID;
         pragma Import (C, LO_Import, "lo_import");
         P : chars_ptr := CS.New_String (In_File_Name);
         Obj_Id : constant OID := LO_Import (To_Addr (C_Connection), P);
      begin
         CS.Free (P);
         return Obj_Id;
      end BLOB_Import;

      -----------------
      -- BLOB_Export --
      -----------------

      function BLOB_Export (Object_Id     : OID;
                            Out_File_Name : String) return Boolean
      is
         function LO_Export (Conn   : System.Address;
                             Obj_Id : OID;
                             File   : chars_ptr) return C.int;
         pragma Import (C, LO_Export, "lo_export");
         P : chars_ptr := CS.New_String (Out_File_Name);
         B : constant Boolean :=
           LO_Export (To_Addr (C_Connection), Object_Id, P) >= 0;
      begin
         CS.Free (P);
         return B;
      end BLOB_Export;

      ---------------
      -- BLOB_Open --
      ---------------

      function BLOB_Open (Object_Id : OID;
                          Mode      : File_Mode)
                          return File_Descriptor
      is
         function LO_Open (Conn   : System.Address;
                           Obj_Id : OID;
                           Mode   : C.int) return Integer;
         pragma Import (C, LO_Open, "lo_open");
      begin
         return File_Descriptor
           (LO_Open (To_Addr (C_Connection), Object_Id, C.int (Mode)));
      end BLOB_Open;

      ----------------
      -- BLOB_Write --
      ----------------

      function BLOB_Write (FD : File_Descriptor;
                           A  : System.Address;
                           N  : Integer)
                           return Integer
      is
         function LO_Write (Conn : System.Address;
                            FD   : C.int;
                            A    : System.Address;
                            N    : C.int) return Integer;
         pragma Import (C, LO_Write, "lo_write");
      begin
         return LO_Write (To_Addr (C_Connection), C.int (FD), A, C.int (N));
      end BLOB_Write;

      ---------------
      -- BLOB_Read --
      ---------------

      function BLOB_Read (FD : File_Descriptor;
                          A  : System.Address;
                          N  : Integer)
                       return Integer
      is
         function LO_Read (Conn : System.Address;
                           FD   : C.int;
                           A    : System.Address;
                           N    : C.int) return Integer;
         pragma Import (C, LO_Read, "lo_read");
      begin
         return LO_Read (To_Addr (C_Connection), C.int (FD), A, C.int (N));
      end BLOB_Read;

      ----------------
      -- BLOB_Lseek --
      ----------------

      function BLOB_Lseek (FD     : File_Descriptor;
                           Offset : Integer;
                           Origin : Integer)
                           return Integer
      is
         function LO_Lseek (Conn   : System.Address;
                            FD     : C.int;
                            Offset : C.int;
                            Origin : C.int) return C.int;
         pragma Import (C, LO_Lseek, "lo_lseek");
      begin
         return Integer
           (LO_Lseek (To_Addr (C_Connection), C.int (FD),
                      C.int (Offset), C.int (Origin)));
      end BLOB_Lseek;

      ---------------
      -- BLOB_Tell --
      ---------------

      function BLOB_Tell (FD : File_Descriptor)
                          return Integer
      is
         function LO_Tell (Conn : System.Address;
                           FD   : C.int) return C.int;
         pragma Import (C, LO_Tell, "lo_tell");
      begin
         return Integer (LO_Tell (To_Addr (C_Connection), C.int (FD)));
      end BLOB_Tell;

      ----------------
      -- BLOB_Close --
      ----------------

      function BLOB_Close (FD : File_Descriptor)
                        return Boolean
      is
         function LO_Close (Conn : System.Address;
                            FD   : C.int) return C.int;
         pragma Import (C, LO_Close, "lo_close");
      begin
         return LO_Close (To_Addr (C_Connection), C.int (FD)) >= 0;
      end BLOB_Close;

      -----------------
      -- BLOB_Unlink --
      -----------------

      function BLOB_Unlink (Object_Id : OID) return Boolean
      is
         function LO_Unlink (Conn          : System.Address;
                             Object_Id     : OID) return C.int;
         pragma Import (C, LO_Unlink, "lo_unlink");
      begin
         return LO_Unlink (To_Addr (C_Connection), Object_Id) >= 0;
      end BLOB_Unlink;

      ------------------
      -- Empty_Result --
      ------------------

      procedure Empty_Result (Res    : out Result;
                              Status : ExecStatus) is
         function PQemptyRes (Conn   : System.Address;
                              Status : C.int) return System.Address;
         pragma Import (C, PQemptyRes, "PQmakeEmptyPGresult");
         R : constant PGresult :=
           To_Result (PQemptyRes (To_Addr (C_Connection),
                                  ExecStatus'Pos (Status)));
      begin
         if R = Null_Result then
            raise PostgreSQL_Error;
         else
            Res.Res := R;
         end if;
      end Empty_Result;

      --------------------
      -- SetNonBlocking --
      --------------------

      procedure SetNonBlocking
      is
         --  function PQnonblocking (Conn : System.Address)
         --                         return Interfaces.C.int;
         --  pragma Import (C, PQnonblocking, "PQsetnonblocking");
      begin
         null;
      end SetNonBlocking;

      -------------------
      -- IsNonBlocking --
      -------------------

      function IsNonBlocking return Boolean
      is
         function PQisnonblocking (Conn : System.Address)
                                   return Interfaces.C.int;
         pragma Import (C, PQisnonblocking, "PQisnonblocking");
         R : constant Interfaces.C.int :=
           PQisnonblocking (To_Addr (C_Connection));
      begin
         if R /= 0 then
            return True;
         else
            return False;
         end if;
      end IsNonBlocking;

      ---------------
      -- SendQuery --
      ---------------

      function SendQuery (Query : String) return Boolean
      is
         function PQsendQuery (Conn : System.Address;
                               Qry  : chars_ptr) return Interfaces.C.int;
         pragma Import (C, PQsendQuery, "PQsendQuery");
         P : constant chars_ptr := CS.New_String (Query);
         R : constant Interfaces.C.int :=
           PQsendQuery (To_Addr (C_Connection), P);
      begin
         if R /= 0 then
            return True;
         else
            return False;
         end if;
      end SendQuery;

      ---------------
      -- GetResult --
      ---------------

      procedure GetResult (Res  : out Result;
                           Done : out Boolean)
      is
         function PQgetResult (Conn : System.Address) return System.Address;
         pragma Import (C, PQgetResult, "PQgetResult");
         R : constant PGresult :=
           To_Result (PQgetResult (To_Addr (C_Connection)));
      begin
         if R = Null_Result then
            Done := True;
         else
            Res.Res := R;
            Done := False;
         end if;
      end GetResult;

      ------------------
      -- ConsumeInput --
      ------------------

      function ConsumeInput return Boolean is
         function PQconsumeInput (Conn : System.Address)
                                  return Interfaces.C.int;
         pragma Import (C, PQconsumeInput, "PQconsumeInput");
         R : constant Interfaces.C.int :=
           PQconsumeInput (To_Addr (C_Connection));
      begin
         if R /= 0 then
            return True;
         else
            return False;
         end if;
      end ConsumeInput;

      -----------
      -- Flush --
      -----------

      function Flush return Boolean is
         function PQflush (Conn : System.Address) return Interfaces.C.int;
         pragma Import (C, PQflush, "PQflush");
         R : constant Interfaces.C.int := PQflush (To_Addr (C_Connection));
      begin
         if R = 0 then
            return True;
         else
            return False;
         end if;
      end Flush;

      ------------
      -- IsBusy --
      ------------

      function IsBusy return Boolean is
         function PQisBusy (Conn : System.Address) return Interfaces.C.int;
         pragma Import (C, PQisBusy, "PQisBusy");
         R : constant Interfaces.C.int := PQisBusy (To_Addr (C_Connection));
      begin
         if R /= 0 then
            return True;
         else
            return False;
         end if;
      end IsBusy;

      -------------------
      -- RequestCancel --
      -------------------

      function RequestCancel return Boolean is
         function PQrequestCancel (Conn : System.Address)
                                   return Interfaces.C.int;
         pragma Import (C, PQrequestCancel, "PQrequestCancel");
         R : constant Interfaces.C.int :=
           PQrequestCancel (To_Addr (C_Connection));
      begin
         if R /= 0 then
            return True;
         else
            return False;
         end if;
      end RequestCancel;

      ------------
      -- Socket --
      ------------

      function Socket return Interfaces.C.int is
         function PQsocket (Conn : System.Address) return Interfaces.C.int;
         pragma Import (C, PQsocket, "PQsocket");
         R : constant Interfaces.C.int := PQsocket (To_Addr (C_Connection));
      begin
         return R;
      end Socket;

      --------------
      -- Notifies --
      --------------

      procedure Notifies (Message : out Notification;
                          Done    : out Boolean)
      is
         function PQnotifies (Conn : System.Address) return System.Address;
         pragma Import (C, PQnotifies, "PQnotifies");
         procedure Free (Addr : System.Address);
         pragma Import (C, Free, "free");
         type NotiPtr is access all Notification;
         package P is new System.Address_To_Access_Conversions (Notification);
         function Cvt is new Ada.Unchecked_Conversion (P.Object_Pointer,
                                                       NotiPtr);
         N : NotiPtr;
         A : constant System.Address := PQnotifies (To_Addr (C_Connection));
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

   end PGConn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Database) is
      pragma Unreferenced (Object);
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Database) is
   begin
      Object.Connection.Close;
      Unchecked_Free (Object.Connection);
   end Finalize;

   function Connect (Params : access String) return Connection_Handle is
      function PQConnect (Options :  char_array) return System.Address;
      pragma Import (C, PQConnect, "PQconnectdb");

      Conn : Connection_Handle;
   begin
      Conn := To_Conn (PQConnect (C.To_C (Params.all)));
      if Conn = Null_Connection then
         raise PostgreSQL_Error;
      else
         return Conn;
      end if;
   end Connect;

   procedure Reset (DB : Database'Class) is
   begin
      DB.Connection.Reset;
   end Reset;

   type C_Accessor is access function (Conn : System.Address) return chars_ptr;
   pragma Convention (C, C_Accessor);

   generic
      Accessor : C_Accessor;
   function Connection_Accessor (DB : Database'Class) return String;

   function  Connection_Accessor (DB : Database'Class) return String is
   begin
      return CS.Value (Accessor.all (To_Addr (DB.Connection.Handle)));
   end Connection_Accessor;

   function PQdb (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQdb, "PQdb");
   function Get_Name is new Connection_Accessor (Accessor => PQdb'Access);

   function PQuser (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQuser, "PQuser");
   function Get_User is new Connection_Accessor (Accessor => PQuser'Access);

   function PQpass (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQpass, "PQpass");
   function Get_Pass is new Connection_Accessor (Accessor => PQpass'Access);

   function PQhost (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQhost, "PQhost");
   function Get_Host is new Connection_Accessor (Accessor => PQhost'Access);

   function PQport (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQport, "PQport");
   function Get_Port is new Connection_Accessor (Accessor => PQport'Access);

   function PQtty (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQtty, "PQtty");
   function Get_TTY is new Connection_Accessor (Accessor => PQtty'Access);

   function PQopt (Conn : System.Address) return chars_ptr;
   pragma Import (C, PQopt, "PQoptions");
   function Get_Options is new Connection_Accessor (Accessor => PQopt'Access);

   function Name     (DB : Database'Class) return String renames Get_Name;
   function User     (DB : Database'Class) return String renames Get_User;
   function Password (DB : Database'Class) return String renames Get_Pass;
   function Host     (DB : Database'Class) return String renames Get_Host;
   function Port     (DB : Database'Class) return String renames Get_Port;
   function TTY      (DB : Database'Class) return String renames Get_TTY;
   function Options  (DB : Database'Class) return String
     renames Get_Options;

   function Error    (DB : Database'Class) return String is
   begin
      return DB.Connection.Error;
   end Error;

   function Status (DB : Database'Class) return ConnStatus is
   begin
      return DB.Connection.Status;
   end Status;

   function Server_PID (DB : Database'Class) return Backend_PID is
   begin
      return DB.Connection.PID;
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
      Success : Boolean;
   begin
      --  Error status is in fact available through Result already
      DB.Connection.Execute (Res, Success, Query, Format, Params);
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
   begin
      DB.Connection.Prepare (Res, Stmt_Name, Query);
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
   begin
      DB.Connection.Exec_Prepared (Res, Stmt_Name, Format, Params);
   end Exec_Prepared;

   -----------------
   -- BLOB_Create --
   -----------------

   function BLOB_Create (DB   : Database'Class;
                         Mode : File_Mode)
                          return OID
   is
   begin
      return DB.Connection.BLOB_Create (Mode);
   end BLOB_Create;

   function BLOB_Import (DB           : Database'Class;
                         In_File_Name : String) return OID
   is
   begin
      return DB.Connection.BLOB_Import (In_File_Name);
   end BLOB_Import;

   function BLOB_Export (DB            : Database'Class;
                         Object_Id     : OID;
                         Out_File_Name : String) return Boolean
   is
   begin
      return DB.Connection.BLOB_Export (Object_Id, Out_File_Name);
   end BLOB_Export;

   function BLOB_Open (DB        : Database'Class;
                       Object_Id : OID;
                       Mode      : File_Mode)
                       return File_Descriptor
   is
   begin
      return DB.Connection.BLOB_Open (Object_Id, Mode);
   end BLOB_Open;

   function BLOB_Write (DB : Database'Class;
                        FD : File_Descriptor;
                        A  : System.Address;
                        N  : Integer)
                        return Integer
   is
   begin
      return DB.Connection.BLOB_Write (FD, A, N);
   end BLOB_Write;

   function BLOB_Read (DB : Database'Class;
                       FD : File_Descriptor;
                       A  : System.Address;
                       N  : Integer)
                       return Integer
   is
   begin
      return DB.Connection.BLOB_Read (FD, A, N);
   end BLOB_Read;

   function BLOB_Lseek (DB     : Database'Class;
                        FD     : File_Descriptor;
                        Offset : Integer;
                        Origin : Integer)
                        return Integer
   is
   begin
      return DB.Connection.BLOB_Lseek (FD, Offset, Origin);
   end BLOB_Lseek;

   function BLOB_Tell (DB : Database'Class;
                       FD : File_Descriptor)
                       return Integer
   is
   begin
      return DB.Connection.BLOB_Tell (FD);
   end BLOB_Tell;

   function BLOB_Close (DB : Database'Class;
                        FD : File_Descriptor)
                        return Boolean
   is
   begin
      return DB.Connection.BLOB_Close (FD);
   end BLOB_Close;

   function BLOB_Unlink (DB        : Database'Class;
                         Object_Id : OID) return Boolean
   is
   begin
      return DB.Connection.BLOB_Unlink (Object_Id);
   end BLOB_Unlink;

   --  -----------------------------------------------------------------------

   procedure Make_Empty_Result (Res    : out Result;
                                DB     : Database'Class;
                                Status : ExecStatus := PGRES_EMPTY_QUERY)
   is
   begin
      DB.Connection .Empty_Result (Res, Status);
   end Make_Empty_Result;

   -----------
   -- Clear --
   -----------

   procedure Clear (Res : in out Result) is
      procedure PQclear (Res : System.Address);
      pragma Import (C, PQclear, "PQclear");
   begin
      if Res.Res /= Null_Result then
         PQclear (To_Addr (Res.Res));
         Res.Res := Null_Result;
      end if;
   end Clear;

   function Status (Res : Result) return ExecStatus is
      function PQresStatus (Conn : System.Address) return Interfaces.C.int;
      pragma Import (C, PQresStatus, "PQresultStatus");
   begin
      return ExecStatus'Val (PQresStatus (To_Addr (Res.Res)));
   end Status;

   function Status (Status : ExecStatus) return String is
      function PQresStat (stat : Interfaces.C.int) return chars_ptr;
      pragma Import (C, PQresStat, "PQresStatus");
   begin
      return CS.Value (PQresStat (ExecStatus'Pos (Status)));
   end Status;

   function Status (Res : Result) return String is
      Stat :  constant ExecStatus := Status (Res);
   begin
      return Status (Stat);
   end Status;

   function Error (Res : Result) return String is
      function PQresErr (Res : System.Address) return chars_ptr;
      pragma Import (C, PQresErr, "PQresultErrorMessage");
   begin
      return CS.Value (PQresErr (To_Addr (Res.Res)));
   end Error;

   function Quote_Identifier (Identifier : String) return String is
   begin
      return '"' & Identifier & '"';
   end Quote_Identifier;

   type C_Info is access function (Res : System.Address) return C.int;
   pragma Convention (C, C_Info);

   generic
      Accessor : C_Info;
   function Info_Accessor (Res : Result) return Integer;

   function  Info_Accessor (Res : Result) return Integer is
   begin
      return Integer (Accessor.all (To_Addr (Res.Res)));
   end Info_Accessor;

   function PQntuples (Res : System.Address) return C.int;
   pragma Import (C, PQntuples, "PQntuples");
   function Get_Count is new Info_Accessor (Accessor => PQntuples'Access);

   function PQnfields (Res : System.Address) return C.int;
   pragma Import (C, PQnfields, "PQnfields");
   function Get_FCount is new Info_Accessor (Accessor => PQnfields'Access);

   function PQbinaryTuples (Res : System.Address) return C.int;
   pragma Import (C, PQbinaryTuples, "PQbinaryTuples");
   function Get_BinaryTuples is new
     Info_Accessor (Accessor => PQbinaryTuples'Access);

   function Tuple_Count (Res : Result) return Tuple_Index is
   begin
      return Tuple_Index (Get_Count (Res));
   end Tuple_Count;

   function Field_Count (Res : Result) return Field_Index is
   begin
      return Field_Index (Get_FCount (Res));
   end Field_Count;

   function Field_Name  (Res   : Result;
                         Index : Field_Index) return String is
      function PQfname (Res : System.Address;
                        Idx : C.int) return chars_ptr;
      pragma Import (C, PQfname, "PQfname");
   begin
      return CS.Value (PQfname (To_Addr (Res.Res),
                                C.int (Index)));
   end Field_Name;

   procedure Field_Lookup (Res   : Result;
                           Name  : String;
                           Index : out Field_Index;
                           Found : out Boolean) is
      function PQfnumber (Res  : System.Address;
                          Name : chars_ptr) return C.int;
      pragma Import (C, PQfnumber, "PQfnumber");
      P : chars_ptr := CS.New_String (Name);
      I : constant C.int := PQfnumber (To_Addr (Res.Res), P);
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

   function Is_Binary (Res   : Result;
                       Index : Field_Index) return Boolean is
      pragma Unreferenced (Index);
      I : constant Integer := Get_BinaryTuples (Res);
   begin
      if I /= 0 then
         return True;
      else
         return False;
      end if;
   end Is_Binary;

   function Field_Type  (Res   : Result;
                         Index : Field_Index) return TypeID is
      function PQftype (Res : System.Address;
                        Idx : C.int) return TypeID;
      pragma Import (C, PQftype, "PQftype");
   begin
      return PQftype (To_Addr (Res.Res), C.int (Index));
   end Field_Type;

   procedure Value (Res     : Result;
                    Tuple   : Tuple_Index;
                    Field   : Field_Index;
                    Pointer : out System.Address)
   is
      function Cvt is new Ada.Unchecked_Conversion (chars_ptr,
                                                    System.Address);
      function PQgetvalue (Res   : System.Address;
                           Tuple : C.int;
                           Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
      P : constant chars_ptr := PQgetvalue (To_Addr (Res.Res),
                                            C.int (Tuple),
                                            C.int (Field));
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
      function PQgetvalue (Res   : System.Address;
                           Tuple : C.int;
                           Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
   begin
      return PQgetvalue (To_Addr (Res.Res),
                         C.int (Tuple),
                         C.int (Field));
   end C_Value;

   -----------
   -- Value --
   -----------

   function Value (Res   : Result;
                   Tuple : Tuple_Index := 0;
                   Field : Field_Index := 0) return String is
      function PQgetvalue (Res   : System.Address;
                           Tuple : C.int;
                           Field : C.int) return chars_ptr;
      pragma Import (C, PQgetvalue, "PQgetvalue");
      P : constant chars_ptr := PQgetvalue (To_Addr (Res.Res),
                                            C.int (Tuple),
                                            C.int (Field));
   begin
      if Is_Binary (Res, Field) then
         raise PostgreSQL_Error;
      end if;
      return CS.Value (P);
   end Value;

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
                   Field_Name : String) return String is
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

   function Field_Size  (Res   : Result;
                         Field : Field_Index) return Integer is
      function PQfsize (Res : System.Address;
                        Idx : C.int) return C.int;
      pragma Import (C, PQfsize, "PQfsize");
   begin
      return Integer (PQfsize (To_Addr (Res.Res), C.int (Field)));
   end Field_Size;

   function Field_Modification (Res   : Result;
                                Field : Field_Index) return Integer is
      function PQfmod (Res : System.Address;
                       Idx : C.int) return C.int;
      pragma Import (C, PQfmod, "PQfmod");
   begin
      return Integer (PQfmod (To_Addr (Res.Res), C.int (Field)));
   end Field_Modification;

   function Field_Length (Res   : Result;
                          Tuple : Tuple_Index;
                          Field : Field_Index) return Natural is
      function PQgetlen (Res : System.Address;
                         Row : C.int;
                         Idx : C.int) return C.int;
      pragma Import (C, PQgetlen, "PQgetlength");
   begin
      return Natural (PQgetlen (To_Addr (Res.Res),
                                C.int (Tuple),
                                C.int (Field)));
   end Field_Length;

   function Is_Null (Res   : Result;
                     Tuple : Tuple_Index;
                     Field : Field_Index) return Boolean is
      function PQisnull (Res : System.Address;
                         Row : C.int;
                         Idx : C.int) return C.int;
      pragma Import (C, PQisnull, "PQgetisnull");

      R : constant C.int := PQisnull (To_Addr (Res.Res),
                                      C.int (Tuple),
                                      C.int (Field));
   begin
      if R = 0 then
         return False;
      else
         return True;
      end if;
   end Is_Null;

   function Command_Status (Res : Result) return String is
      function PQcmdStatus (Res : System.Address) return chars_ptr;
      pragma Import (C, PQcmdStatus, "PQcmdStatus");
   begin
      return CS.Value (PQcmdStatus (To_Addr (Res.Res)));
   end Command_Status;

   function Command_Tuples (Res : Result) return String is
      function PQcmdTuples (Res : System.Address) return chars_ptr;
      pragma Import (C, PQcmdTuples, "PQcmdTuples");
   begin
      return CS.Value (PQcmdTuples (To_Addr (Res.Res)));
   end Command_Tuples;

   function Command_Tuples (Res : Result) return Natural is
      S : constant String := Command_Tuples (Res);
   begin
      if S = "" then
         return 0;
      else
         return Natural'Value (S);
      end if;
   end Command_Tuples;

   function OID_Value (Res : Result) return OID is
      function PQoidValue (Res : System.Address) return OID;
      pragma Import (C, PQoidValue, "PQoidValue");
   begin
      return PQoidValue (To_Addr (Res.Res));
   end OID_Value;

   procedure Set_Non_Blocking (DB : Database'Class) is
   begin
      DB.Connection.SetNonBlocking;
   end Set_Non_Blocking;

   function Is_Non_Blocking (DB : Database'Class) return Boolean
   is
   begin
      return DB.Connection.IsNonBlocking;
   end Is_Non_Blocking;

   function Send_Query (DB    : Database'Class;
                        Query : String) return Boolean
   is
   begin
      return DB.Connection.SendQuery (Query);
   end Send_Query;

   procedure Get_Result (DB  :  Database'Class;
                         Res :  out Result;
                         Done : out Boolean)
   is
   begin
      DB.Connection.GetResult (Res, Done);
   end Get_Result;

   function Consume_Input (DB : Database'Class) return Boolean
   is
   begin
      return DB.Connection.ConsumeInput;
   end Consume_Input;

   function Flush (DB : Database'Class) return Boolean
   is
   begin
      return DB.Connection.Flush;
   end Flush;

   function  Is_Busy (DB : Database'Class) return Boolean
   is
   begin
      return DB.Connection.IsBusy;
   end Is_Busy;

   function  Request_Cancel (DB : Database'Class) return Boolean
   is
   begin
      return DB.Connection.RequestCancel;
   end Request_Cancel;

   function  Socket (DB : Database'Class) return Interfaces.C.int
   is
   begin
      return DB.Connection.Socket;
   end Socket;

   procedure Notifies (DB      : Database'Class;
                       Message : out Notification;
                       Done    : out Boolean)
   is
   begin
      DB.Connection.Notifies (Message, Done);
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
