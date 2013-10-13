with GNATCOLL.SQL; use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;
with GNATCOLL.Xref.Database_Names; use GNATCOLL.Xref.Database_Names;
package GNATCOLL.Xref.Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   subtype F2f_Id is Integer;
   F2f_Has_Ali : constant F2f_Id := 1;
   F2f_Withs : constant F2f_Id := 0;

   subtype E2e_Id is Integer;
   E2e_Access_Parameter : constant E2e_Id := 15;
   E2e_Component_Type : constant E2e_Id := 9;
   E2e_From_Enumeration : constant E2e_Id := 10;
   E2e_Has_Discriminant : constant E2e_Id := 16;
   E2e_Has_Index : constant E2e_Id := 7;
   E2e_Has_Primitive : constant E2e_Id := 4;
   E2e_In_Out_Parameter : constant E2e_Id := 14;
   E2e_In_Parameter : constant E2e_Id := 12;
   E2e_Instance_Of : constant E2e_Id := 5;
   E2e_Is_Formal_Of : constant E2e_Id := 17;
   E2e_Is_Param_Of : constant E2e_Id := 6;
   E2e_Of_Type : constant E2e_Id := 11;
   E2e_Out_Parameter : constant E2e_Id := 13;
   E2e_Overrides : constant E2e_Id := 8;
   E2e_Parent_Package : constant E2e_Id := 18;
   E2e_Parent_Type : constant E2e_Id := 1;
   E2e_Pointed_Type : constant E2e_Id := 0;
   E2e_Renames : constant E2e_Id := 3;

   type T_Abstract_E2e (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_E2e, Instance, Index) with
   record
      Fromentity : SQL_Field_Integer (Ta_E2e, Instance, N_Fromentity, Index);
      Toentity : SQL_Field_Integer (Ta_E2e, Instance, N_Toentity, Index);
      Kind : SQL_Field_Integer (Ta_E2e, Instance, N_Kind, Index);
      --  The type of link.

      Order_By : SQL_Field_Integer (Ta_E2e, Instance, N_Order_By, Index);
      --  Ordering among the references. Used for instance for subprogram
      --  parameters

   end record;

   type T_E2e (Instance : Cst_String_Access)
      is new T_Abstract_E2e (Instance, -1) with null record;
   type T_Numbered_E2e (Index : Integer)
      is new T_Abstract_E2e (null, Index) with null record;

   type T_Abstract_E2e_Kind (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_E2e_Kind, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_E2e_Kind, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_E2e_Kind, Instance, N_Name, Index);
   end record;

   type T_E2e_Kind (Instance : Cst_String_Access)
      is new T_Abstract_E2e_Kind (Instance, -1) with null record;
   type T_Numbered_E2e_Kind (Index : Integer)
      is new T_Abstract_E2e_Kind (null, Index) with null record;

   type T_Abstract_Entities (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Entities, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Entities, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Entities, Instance, N_Name, Index);
      --  Can be empty for forward decls (see above). Must be UTF-8

      Kind : SQL_Field_Text (Ta_Entities, Instance, N_Kind, Index);
      --  The E_Kind for this entity

      Decl_File : SQL_Field_Integer (Ta_Entities, Instance, N_Decl_File, Index);
      --  Set to -1 for a predefined entity

      Decl_Line : SQL_Field_Integer (Ta_Entities, Instance, N_Decl_Line, Index);
      --  Set to -1 for a predefined entity

      Decl_Column : SQL_Field_Integer (Ta_Entities, Instance, N_Decl_Column, Index);
      --  Set to -1 for a predefined entity

      Decl_Caller : SQL_Field_Integer (Ta_Entities, Instance, N_Decl_Caller, Index);
      --  Parent entity

      Mangled_Name : SQL_Field_Text (Ta_Entities, Instance, N_Mangled_Name, Index);
      --  Mangled name of the entity, if applicable

      Exported : SQL_Field_Boolean (Ta_Entities, Instance, N_Exported, Index);
      --  Whether the mangled name is an export or an import of the entity

      Is_Global : SQL_Field_Boolean (Ta_Entities, Instance, N_Is_Global, Index);
      --  Whether this is a global entity (library-level in Ada)

      Is_Static_Local : SQL_Field_Boolean (Ta_Entities, Instance, N_Is_Static_Local, Index);
      --  Whether this is a 'static' variable in C/C++

   end record;

   type T_Entities (Instance : Cst_String_Access)
      is new T_Abstract_Entities (Instance, -1) with null record;
   type T_Numbered_Entities (Index : Integer)
      is new T_Abstract_Entities (null, Index) with null record;

   type T_Abstract_Entity_Kinds (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Entity_Kinds, Instance, Index) with
   record
      Id : SQL_Field_Text (Ta_Entity_Kinds, Instance, N_Id, Index);
      Display : SQL_Field_Text (Ta_Entity_Kinds, Instance, N_Display, Index);
      --  How to display this entity kind

      Is_Subprogram : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Subprogram, Index);
      Is_Container : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Container, Index);
      Body_Is_Full_Declaration : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Body_Is_Full_Declaration, Index);
      Is_Abstract : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Abstract, Index);
      Is_Generic : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Generic, Index);
      Is_Access : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Access, Index);
      Is_Type : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Type, Index);
      Is_Printable_In_Gdb : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Printable_In_Gdb, Index);
      Is_Array : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Is_Array, Index);
      Has_Methods : SQL_Field_Boolean (Ta_Entity_Kinds, Instance, N_Has_Methods, Index);
   end record;

   type T_Entity_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Entity_Kinds (Instance, -1) with null record;
   type T_Numbered_Entity_Kinds (Index : Integer)
      is new T_Abstract_Entity_Kinds (null, Index) with null record;

   type T_Abstract_Entity_Refs (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Entity_Refs, Instance, Index) with
   record
      Entity : SQL_Field_Integer (Ta_Entity_Refs, Instance, N_Entity, Index);
      --  The entity to which we have a reference

      File : SQL_Field_Integer (Ta_Entity_Refs, Instance, N_File, Index);
      Line : SQL_Field_Integer (Ta_Entity_Refs, Instance, N_Line, Index);
      Column : SQL_Field_Integer (Ta_Entity_Refs, Instance, N_Column, Index);
      Kind : SQL_Field_Text (Ta_Entity_Refs, Instance, N_Kind, Index);
      --  Type of reference (same letter as in ALI files)

      Caller : SQL_Field_Integer (Ta_Entity_Refs, Instance, N_Caller, Index);
      --  Enclosing entity at that location

      From_Instantiation : SQL_Field_Text (Ta_Entity_Refs, Instance, N_From_Instantiation, Index);
      --  Instances in which the ref occurs

   end record;

   type T_Entity_Refs (Instance : Cst_String_Access)
      is new T_Abstract_Entity_Refs (Instance, -1) with null record;
   type T_Numbered_Entity_Refs (Index : Integer)
      is new T_Abstract_Entity_Refs (null, Index) with null record;

   type T_Abstract_F2f (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_F2f, Instance, Index) with
   record
      Fromfile : SQL_Field_Integer (Ta_F2f, Instance, N_Fromfile, Index);
      Tofile : SQL_Field_Integer (Ta_F2f, Instance, N_Tofile, Index);
      Kind : SQL_Field_Integer (Ta_F2f, Instance, N_Kind, Index);
      --  Kind of dependency

   end record;

   type T_F2f (Instance : Cst_String_Access)
      is new T_Abstract_F2f (Instance, -1) with null record;
   type T_Numbered_F2f (Index : Integer)
      is new T_Abstract_F2f (null, Index) with null record;

   type T_Abstract_F2f_Kind (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_F2f_Kind, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_F2f_Kind, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_F2f_Kind, Instance, N_Name, Index);
   end record;

   type T_F2f_Kind (Instance : Cst_String_Access)
      is new T_Abstract_F2f_Kind (Instance, -1) with null record;
   type T_Numbered_F2f_Kind (Index : Integer)
      is new T_Abstract_F2f_Kind (null, Index) with null record;

   type T_Abstract_Files (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Files, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Files, Instance, N_Id, Index);
      Path : SQL_Field_Text (Ta_Files, Instance, N_Path, Index);
      --  Full normalized absolute path for the file

      Stamp : SQL_Field_Time (Ta_Files, Instance, N_Stamp, Index);
      --  The timestamp the last time the file was read (only set for LI files
      --  for efficiency)

      Language : SQL_Field_Text (Ta_Files, Instance, N_Language, Index);
      --  The language for this file (so that we can limit queries to specific
      --  languages), or "li"

   end record;

   type T_Files (Instance : Cst_String_Access)
      is new T_Abstract_Files (Instance, -1) with null record;
   type T_Numbered_Files (Index : Integer)
      is new T_Abstract_Files (null, Index) with null record;

   type T_Abstract_Reference_Kinds (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Reference_Kinds, Instance, Index) with
   record
      Id : SQL_Field_Text (Ta_Reference_Kinds, Instance, N_Id, Index);
      --  The character found in the ALI file

      Display : SQL_Field_Text (Ta_Reference_Kinds, Instance, N_Display, Index);
      --  Label to display the reference

      Is_Real : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_Real, Index);
      --  Whether the name of the entity appears at that location

      Is_Read : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_Read, Index);
      Is_Write : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_Write, Index);
      Is_End : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_End, Index);
      --  Whether this marks the end of a scope (spec or body)

      Show_In_Callgraph : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Show_In_Callgraph, Index);
      --  Whether this ref. should be shown in the call graph

      Is_Dispatching : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_Dispatching, Index);
      --  Whether this is a dispatching call

      Is_Implicit : SQL_Field_Boolean (Ta_Reference_Kinds, Instance, N_Is_Implicit, Index);
   end record;

   type T_Reference_Kinds (Instance : Cst_String_Access)
      is new T_Abstract_Reference_Kinds (Instance, -1) with null record;
   type T_Numbered_Reference_Kinds (Index : Integer)
      is new T_Abstract_Reference_Kinds (null, Index) with null record;

   function FK (Self : T_E2e'Class; Foreign : T_E2e_Kind'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Entity_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Files'Class) return SQL_Criteria;
   function FK (Self : T_Entities'Class; Foreign : T_Entities'Class) return SQL_Criteria;
   function FK (Self : T_Entity_Refs'Class; Foreign : T_Files'Class) return SQL_Criteria;
   function FK (Self : T_Entity_Refs'Class; Foreign : T_Reference_Kinds'Class) return SQL_Criteria;
   function FK (Self : T_F2f'Class; Foreign : T_F2f_Kind'Class) return SQL_Criteria;

   E2e : T_E2e (null);
   E2e_Kind : T_E2e_Kind (null);
   Entities : T_Entities (null);
   Entity_Kinds : T_Entity_Kinds (null);
   Entity_Refs : T_Entity_Refs (null);
   F2f : T_F2f (null);
   F2f_Kind : T_F2f_Kind (null);
   Files : T_Files (null);
   Reference_Kinds : T_Reference_Kinds (null);

   procedure Create_Database
      (DB : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class);
   --  Create the database and its initial contents
   --  The SQL is not automatically committed
end GNATCOLL.Xref.Database;
