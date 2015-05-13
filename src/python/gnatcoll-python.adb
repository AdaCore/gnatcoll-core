------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with System;               use System;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.OS_Lib;          use GNAT.OS_Lib;

package body GNATCOLL.Python is

   No_Method_Def : constant PyMethodDef :=
     (Name  => Null_Ptr,
      Func  => null,
      Flags => METH_VARGS or METH_KEYWORDS,
      Doc   => Null_Ptr);

   type Methods_Access is access PyMethodDef_Array;
   type MethodDef_Access is access PyMethodDef;
   pragma Convention (C, MethodDef_Access);

   function PyCFunction_New
     (MethodDef : MethodDef_Access;
      Self      : PyObject;
      Module    : PyObject := null) return PyObject;
   pragma Import (C, PyCFunction_New, "PyCFunction_NewEx");
   --  Create a new callable object, which, when called from python, will call
   --  the Ada subprogram.
   --  Self is the first argument that will be passed to the Ada subprogram.
   --  Module is the value of the __module__ attribute for the new function.

   ------------------------
   -- PyRun_SimpleString --
   ------------------------

   function PyRun_SimpleString (Cmd : String) return Boolean is
      function Internal (Cmd : String) return Integer;
      pragma Import (C, Internal, "PyRun_SimpleString");
   begin
      return Internal (Cmd & ASCII.NUL) = 0;
   end PyRun_SimpleString;

   ------------------------
   -- PyImport_AddModule --
   ------------------------

   function PyImport_AddModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_AddModule");
   begin
      return Internal (Module_Name & ASCII.NUL);
   end PyImport_AddModule;

   ---------------------------
   -- PyImport_ImportModule --
   ---------------------------

   function PyImport_ImportModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_ImportModule");
   begin
      return Internal (Module_Name & ASCII.NUL);
   end PyImport_ImportModule;

   ------------------
   -- PyRun_String --
   ------------------

   function PyRun_String
     (Str     : String;
      Start   : Interpreter_State;
      Globals : PyObject;
      Locals  : PyObject) return PyObject
   is
      function Internal
        (Str     : String;
         Start   : Interpreter_State;
         Globals : PyObject;
         Locals  : PyObject) return PyObject;
      pragma Import (C, Internal, "PyRun_String");
   begin
      return Internal (Str & ASCII.LF, Start, Globals, Locals);
   end PyRun_String;

   ----------------------
   -- PyArg_ParseTuple --
   ----------------------

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1 : System.Address) return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr2");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr3");
   begin
      return Internal (Arg, Format & ASCII.NUL, Value1, Value2, Value3) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr4");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4) = 1;
   end PyArg_ParseTuple;

   function PyArg_ParseTuple
     (Arg    : PyObject;
      Format : String;
      Value1, Value2, Value3, Value4, Value5 : System.Address) return Boolean
   is
      function Internal
        (Arg : PyObject; Format : String; V1, V2, V3, V4, V5 : System.Address)
         return Integer;
      pragma Import (C, Internal, "ada_py_arg_parsetuple_ptr5");
   begin
      return Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4, Value5) = 1;
   end PyArg_ParseTuple;

   ----------------------
   -- PyFunction_Check --
   ----------------------

   function PyFunction_Check (Func : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyfunction_check");
   begin
      return Internal (Func) = 1;
   end PyFunction_Check;

   ----------------------
   -- PyCallable_Check --
   ----------------------

   function PyCallable_Check (Func : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyCallable_Check");
   begin
      return Internal (Func) = 1;
   end PyCallable_Check;

   --------------------
   -- PyString_Check --
   --------------------

   function PyString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pystring_check");
   begin
      return Internal (Obj) = 1;
   end PyString_Check;

   ---------------------
   -- PyUnicode_Check --
   ---------------------

   function PyUnicode_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyunicode_check");
   begin
      return Internal (Obj) = 1;
   end PyUnicode_Check;

   ------------------------
   -- PyBaseString_Check --
   ------------------------

   function PyBaseString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybasestring_check");
   begin
      return Internal (Obj) = 1;
   end PyBaseString_Check;

   ------------------
   -- PyList_Check --
   ------------------

   function PyList_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pylist_check");
   begin
      return Internal (Obj) = 1;
   end PyList_Check;

   ------------------
   -- PyIter_Check --
   ------------------

   function PyIter_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyiter_check");
   begin
      return Internal (Obj) = 1;
   end PyIter_Check;

   -----------------
   -- PyInt_Check --
   -----------------

   function PyInt_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyint_check");
   begin
      return Internal (Obj) = 1;
   end PyInt_Check;

   -------------------
   -- PyFloat_Check --
   -------------------

   function PyFloat_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyfloat_check");
   begin
      return Internal (Obj) = 1;
   end PyFloat_Check;

   ------------------------
   -- PyBool_FromBoolean --
   ------------------------

   function PyBool_FromBoolean (Value : Boolean) return PyObject is
      function PyTrue return PyObject;
      pragma Import (C, PyTrue, "ada_py_true");

      function PyFalse return PyObject;
      pragma Import (C, PyFalse, "ada_py_false");

      Result : PyObject;
   begin
      if Value then
         Result := PyTrue;
      else
         Result := PyFalse;
      end if;

      Py_INCREF (Result);
      return Result;
   end PyBool_FromBoolean;

   ------------------
   -- PyBool_Check --
   ------------------

   function PyBool_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybool_check");
   begin
      return Internal (Obj) = 1;
   end PyBool_Check;

   --------------------
   -- PyBool_Is_True --
   --------------------

   function PyBool_Is_True (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybool_is_true");
   begin
      return Internal (Obj) = 1;
   end PyBool_Is_True;

   -------------------
   -- PyTuple_Check --
   -------------------

   function PyTuple_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pytuple_check");
   begin
      return Internal (Obj) = 1;
   end PyTuple_Check;

   ----------------------
   -- PyObject_GetItem --
   ----------------------

   function PyObject_GetItem (Obj : PyObject; Key : Integer) return PyObject is
      K      : PyObject;
      Result : PyObject;
   begin
      K := PyInt_FromLong (Interfaces.C.long (Key));
      Result := PyObject_GetItem (Obj, K);
      Py_DECREF (K);
      return Result;
   end PyObject_GetItem;

   ----------------------
   -- PyObject_SetItem --
   ----------------------

   procedure PyObject_SetItem
     (Obj : PyObject; Key : Integer; Value : PyObject)
   is
      K      : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      K := PyInt_FromLong (Interfaces.C.long (Key));
      Result := PyObject_SetItem (Obj, K, Value);
      Py_DECREF (K);
   end PyObject_SetItem;

   -----------------------
   -- PyString_AsString --
   -----------------------

   function PyString_AsString (Str : PyObject) return String is
      function Low (Str : PyObject) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Low, "ada_PyString_AsString");
      --  Returns a NULL terminated representation of the contents of string.
      --  Result value must be freed.

      C : constant Interfaces.C.Strings.chars_ptr := Low (Str);
   begin
      if C = Null_Ptr then
         return "";
      else
         declare
            R : constant String := Value (C);

            procedure C_Free (S : chars_ptr);
            pragma Import (C, C_Free, "free");

         begin
            --  Since C was allocated by ada_PyString_AsString via strdup(),
            --  and not via System.Memory, we should not be using
            --  Interfaces.C.Strings.Free which goes through System.Memory.
            --  So we call free() directly instead.
            C_Free (C);
            return R;
         end;
      end if;
   end PyString_AsString;

   -------------------------
   -- PyString_FromString --
   -------------------------

   function PyString_FromString (Str : String) return PyObject is
      function Internal (Str : String; Size : Integer) return PyObject;
      pragma Import (C, Internal, "PyString_FromStringAndSize");
   begin
      return Internal (Str, Str'Length);
   end PyString_FromString;

   --------------------------
   -- PyUnicode_FromString --
   --------------------------

   function PyUnicode_FromString (Str : String) return PyObject is
      function Internal (Str : String) return PyObject;
      pragma Import (C, Internal, "ada_PyUnicode_FromString");
   begin
      return Internal (Str & ASCII.NUL);
   end PyUnicode_FromString;

   -------------------------------
   -- PyUnicode_AsEncodedString --
   -------------------------------

   function PyUnicode_AsEncodedString
     (Unicode  : PyObject;
      Encoding : String;
      Errors   : Unicode_Error_Handling := Strict)
      return PyObject
   is
      function Internal
        (Unicode : PyObject; Encoding, Errors : String) return PyObject;
      pragma Import (C, Internal, "ada_PyUnicode_AsEncodedString");
   begin
      case Errors is
         when Strict =>
            return Internal
              (Unicode, Encoding & ASCII.NUL, "strict" & ASCII.NUL);
         when Ignore =>
            return Internal
              (Unicode, Encoding & ASCII.NUL, "ignore" & ASCII.NUL);
         when Replace =>
            return Internal
              (Unicode, Encoding & ASCII.NUL, "replace" & ASCII.NUL);
      end case;
   end PyUnicode_AsEncodedString;

   ----------------------
   -- Unicode_AsString --
   ----------------------

   function Unicode_AsString
     (Str : PyObject; Encoding : String := "utf-8") return String
   is
      S : constant PyObject := PyUnicode_AsEncodedString
        (Unicode => Str, Encoding => Encoding, Errors => Replace);
      Result : constant String := PyString_AsString (S);
   begin
      Py_DECREF (S);
      return Result;
   end Unicode_AsString;

   ---------------------
   -- PySys_SetObject --
   ---------------------

   procedure PySys_SetObject (Name : String; Object : PyObject) is
      procedure Internal (Name : String; Object : PyObject);
      pragma Import (C, Internal, "PySys_SetObject");
   begin
      Internal (Name & ASCII.NUL, Object);
   end PySys_SetObject;

   ---------------------
   -- PySys_GetObject --
   ---------------------

   function PySys_GetObject (Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PySys_GetObject");
   begin
      return Internal (Name & ASCII.NUL);
   end PySys_GetObject;

   -------------------------
   -- PyObject_CallMethod --
   -------------------------

   function PyObject_CallMethod
     (Object : PyObject; Name : String) return PyObject
   is
      function Internal (Object : PyObject; Name : String) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod");
   begin
      return Internal (Object, Name & ASCII.NUL);
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_obj");
   begin
      return Internal (Object, Name & ASCII.NUL, Arg1);
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : Integer) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_int");
   begin
      return Internal (Object, Name & ASCII.NUL, Arg1);
   end PyObject_CallMethod;

   -----------------------
   -- Py_SetProgramName --
   -----------------------

   procedure Py_SetProgramName (Name : String) is
      procedure Internal (Name : String);
      pragma Import (C, Internal, "Py_SetProgramName");

      Program_Name : constant String_Access := new String'(Name & ASCII.NUL);
      --  As stated by the Python documentation the string passed to
      --  Py_SetProgramName should be in "static storage whose contents will
      --  not change for the duration of the program's execution"
   begin
      Internal (Program_Name.all);
   end Py_SetProgramName;

   ----------------------
   -- Py_SetPythonHome --
   ----------------------

   procedure Py_SetPythonHome (Home : String) is
      procedure Internal (Name : String);
      pragma Import (C, Internal, "Py_SetPythonHome");

      C_Home : constant String_Access := new String'(Home & ASCII.NUL);
      --  As stated by the Python documentation the string passed to
      --  Py_SetPythonHome should be in "static storage whose contents will
      --  not change for the duration of the program's execution"
   begin
      Internal (C_Home.all);
   end Py_SetPythonHome;

   ----------------------
   -- Py_CompileString --
   ----------------------

   function Py_CompileString
     (Cmd : String; Name : String; State : Interpreter_State)
      return PyCodeObject
   is
      function Internal (Cmd, Name : String; State : Interpreter_State)
         return PyCodeObject;
      pragma Import (C, Internal, "Py_CompileString");
   begin
      return Internal (Cmd & ASCII.NUL, Name & ASCII.NUL, State);
   end Py_CompileString;

   ------------------
   -- PyDict_Check --
   ------------------

   function PyDict_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pydict_check");
   begin
      return Internal (Obj) /= 0;
   end PyDict_Check;

   --------------------
   -- PyAnySet_Check --
   --------------------

   function PyAnySet_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyanyset_check");
   begin
      return Internal (Obj) /= 0;
   end PyAnySet_Check;

   --------------------------
   -- PyDict_SetItemString --
   --------------------------

   procedure PyDict_SetItemString
     (Dict : PyDictObject; Key : String; Obj : PyObject)
   is
      S      : chars_ptr := New_String (Key);
      Result : constant Integer := PyDict_SetItemString (Dict, S, Obj);
      pragma Unreferenced (Result);
   begin
      Free (S);
   end PyDict_SetItemString;

   ------------------------
   -- PyModule_AddObject --
   ------------------------

   function PyModule_AddObject
     (Module : PyObject; Name : String;  Object : PyObject) return Integer
   is
      S      : chars_ptr := New_String (Name);
      Result : Integer;
   begin
      Result := PyModule_AddObject (Module, S, Object);
      Free (S);
      return Result;
   end PyModule_AddObject;

   --------------------------
   -- PyDict_GetItemString --
   --------------------------

   function PyDict_GetItemString
     (Dict : PyDictObject; Key : String) return PyObject
   is
      S      : chars_ptr := New_String (Key);
      Result : constant PyObject := PyDict_GetItemString (Dict, S);
   begin
      Free (S);
      return Result;
   end PyDict_GetItemString;

   ------------------
   -- Create_Tuple --
   ------------------

   function Create_Tuple (Objects : PyObject_Array) return PyObject is
      Tuple : constant PyObject := PyTuple_New (Objects'Length);
   begin
      for O in Objects'Range loop
         PyTuple_SetItem (Tuple, O - Objects'First, Objects (O));
      end loop;
      return Tuple;
   end Create_Tuple;

   ------------------------
   -- PyErr_NewException --
   ------------------------

   function PyErr_NewException
     (Name : String; Base : PyObject := null; Dict : PyObject := null)
      return PyObject
   is
      function Internal (Name : String; Base, Dict : PyObject) return PyObject;
      pragma Import (C, Internal, "PyErr_NewException");
   begin
      return Internal (Name & ASCII.NUL, Base, Dict);
   end PyErr_NewException;

   ---------------------
   -- PyErr_SetString --
   ---------------------

   procedure PyErr_SetString (Except : PyObject; Msg : String) is
      procedure Internal (Except : PyObject; Msg : String);
      pragma Import (C, Internal, "PyErr_SetString");
   begin
      Internal (Except, Msg & ASCII.NUL);
   end PyErr_SetString;

   ----------------------------
   -- PyObject_GetAttrString --
   ----------------------------

   function PyObject_GetAttrString
     (Object : PyObject; Name : String) return PyObject
   is
      S : chars_ptr := New_String (Name);
      Result : constant PyObject := PyObject_GetAttrString (Object, S);
   begin
      Free (S);
      return Result;
   end PyObject_GetAttrString;

   ----------------------------
   -- PyObject_HasAttrString --
   ----------------------------

   function PyObject_HasAttrString
     (Obj : PyObject; Attr_Name : String) return Boolean
   is
      function Internal (Object : PyObject; S : String) return Integer;
      pragma Import (C, Internal, "PyObject_HasAttrString");
   begin
      return Boolean'Val (Internal (Obj, Attr_Name & ASCII.NUL));
   end PyObject_HasAttrString;

   ----------------------------
   -- PyObject_SetAttrString --
   ----------------------------

   procedure PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject)
   is
      procedure Internal (Obj : PyObject; Name : String; Val : PyObject);
      pragma Import (C, Internal, "PyObject_SetAttrString");
   begin
      Internal (Obj, Attr_Name & ASCII.NUL, Value);
   end PyObject_SetAttrString;

   ----------------------------
   -- PyObject_SetAttrString --
   ----------------------------

   function PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject) return Integer
   is
      function Internal
        (Obj : PyObject; Name : String; Val : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_SetAttrString");
   begin
      return Internal (Obj, Attr_Name & ASCII.NUL, Value);
   end PyObject_SetAttrString;

   -----------------------------------
   -- PyObject_GenericSetAttrString --
   -----------------------------------

   function PyObject_GenericSetAttrString
     (Object : PyObject;
      Name   : String;
      Attr   : PyObject) return Integer
   is
      N : constant PyObject := PyString_FromString (Name);
      Result : Integer;
   begin
      Result := PyObject_GenericSetAttr (Object, N, Attr);
      Py_DECREF (N);
      return Result;
   end PyObject_GenericSetAttrString;

   -----------------
   -- PyDict_Next --
   -----------------

   procedure PyDict_Next
     (Dict  : PyObject;
      Pos   : in out Integer;
      Key   : out PyObject;
      Value : out PyObject)
   is
      function Internal
        (Dict : PyObject; Pos, Key, Value : System.Address) return Integer;
      pragma Import (C, Internal, "PyDict_Next");

      P : Interfaces.C.size_t := Interfaces.C.size_t (Pos);

   begin
      if Internal (Dict, P'Address, Key'Address, Value'Address) = 0 then
         Pos := -1;
      else
         Pos := Integer (P);
      end if;
   end PyDict_Next;

   --------------------
   -- Print_Refcount --
   --------------------

   procedure Print_Refcount (Obj : PyObject; Msg : String) is
      procedure Internal (Obj : PyObject; Msg : String);
      pragma Import (C, Internal, "ada_py_print_refcount");
   begin
      Internal (Obj, Msg & ASCII.NUL);
   end Print_Refcount;

   ------------------------
   -- PyFile_WriteString --
   ------------------------

   function PyFile_WriteString
     (Text : String; File : PyObject) return Boolean
   is
      function Internal (Text : String; File : PyObject) return Integer;
      pragma Import (C, Internal, "PyFile_WriteString");
   begin
      return Internal (Text & ASCII.NUL, File) /= 0;
   end PyFile_WriteString;

   -----------------------
   -- PyFile_FromString --
   -----------------------

   function PyFile_FromString (File_Name, Mode : String) return PyObject is
      function Internal (N, M : String) return PyObject;
      pragma Import (C, Internal, "PyFile_FromString");
   begin
      return Internal (File_Name & ASCII.NUL, Mode & ASCII.NUL);
   end PyFile_FromString;

   -------------------
   -- Py_InitModule --
   -------------------

   function Py_InitModule
     (Module_Name : String;
      Methods     : PyMethodDef_Array := No_MethodDef_Array;
      Doc         : String := "") return PyObject
   is
      function Internal
        (N       : String;
         Methods : System.Address;
         Doc     : String;
         Self    : PyObject := null) return PyObject;
      pragma Import (C, Internal, "ada_Py_InitModule4");

      M : Methods_Access;
   begin
      if Methods /= No_MethodDef_Array then
         --  ??? Memory is never freed, but Python is not supposed to be killed
         --  before the end of the application
         M := new PyMethodDef_Array'(Methods & No_Method_Def);
         return Internal
           (Module_Name & ASCII.NUL, M.all'Address,
            Doc & ASCII.NUL);

      else
         return Internal
           (Module_Name & ASCII.NUL, System.Null_Address,
            Doc & ASCII.NUL);
      end if;
   end Py_InitModule;

   ----------
   -- Free --
   ----------

   procedure Free (Method : in out PyMethodDef) is
      procedure C_Free (C : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, C_Free, "free");

   begin
      C_Free (Method.Name);
      C_Free (Method.Doc);
      Method.Name := Null_Ptr;
      Method.Doc := Null_Ptr;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Methods : in out PyMethodDef_Array) is
   begin
      for M in Methods'Range loop
         Free (Methods (M));
      end loop;
   end Free;

   ------------------
   -- PyModule_New --
   ------------------

   function PyModule_New (Module_Name : String) return PyObject is
      function Internal (N : String) return PyObject;
      pragma Import (C, Internal, "PyModule_New");
   begin
      return Internal (Module_Name & ASCII.NUL);
   end PyModule_New;

   ----------------------
   -- PyModule_Getname --
   ----------------------

   function PyModule_Getname (Module : PyObject) return String is
      function Internal (M : PyObject) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "PyModule_GetName");
   begin
      return Value (Internal (Module));
   end PyModule_Getname;

   ------------------
   -- Add_Function --
   ------------------

   procedure Add_Function
     (Module : PyObject; Func : PyMethodDef; Self : PyObject := null)
   is
      C_Func : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      if Self /= null then
         C_Func := PyCFunction_New
           (new PyMethodDef'(Func), Self,
            PyString_FromString (PyModule_Getname (Module)));
      else
         C_Func := PyCFunction_New
           (new PyMethodDef'(Func), Module,
            PyString_FromString (PyModule_Getname (Module)));
      end if;

      if C_Func /= null then
         Result := PyModule_AddObject (Module, Func.Name, C_Func);
      end if;
   end Add_Function;

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (Class : PyObject;
      Func  : PyMethodDef;
      Self  : PyObject := null;
      Module : PyObject)
   is
      procedure Add_Method
        (Func   : access PyMethodDef;
         Self   : PyObject;
         Class  : PyObject;
         Module : PyObject);
      pragma Import (C, Add_Method, "ada_py_add_method");
   begin
      Add_Method (new PyMethodDef'(Func), Self, Class, Module);
   end Add_Method;

   -----------------------
   -- Add_Static_Method --
   -----------------------

   procedure Add_Static_Method
     (Class : PyObject; Func : PyMethodDef; Self : PyObject := null;
      Module : PyObject)
   is
      function PyStaticMethod_New (Method : PyObject) return PyObject;
      pragma Import (C, PyStaticMethod_New, "PyStaticMethod_New");

      Def    : constant MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Static : PyObject;
      Result : Integer;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_STATIC;

      C_Func := PyCFunction_New
        (Def, Self, PyString_FromString (PyModule_Getname (Module)));
      if C_Func /= null then
         --  ??? Likely not needed for python3
         Static := PyStaticMethod_New (C_Func);
         Result := PyObject_SetAttrString (Class, Func.Name, Static);
         Py_DECREF (Static);
      end if;
   end Add_Static_Method;

   ----------------------
   -- Add_Class_Method --
   ----------------------

   procedure Add_Class_Method
     (Class : PyObject; Func : PyMethodDef; Module : PyObject)
   is
      function PyClassMethod_New (Method : PyObject) return PyObject;
      pragma Import (C, PyClassMethod_New, "PyClassMethod_New");

      Def    : constant MethodDef_Access := new PyMethodDef'(Func);
      C_Func : PyObject;
      Result : Integer;
      Meth   : PyObject;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_CLASS;

      C_Func := PyCFunction_New
        (Def, null, PyString_FromString (PyModule_Getname (Module)));
      if C_Func /= null then
         Meth := PyClassMethod_New (C_Func);
         Result := PyObject_SetAttrString (Class, Func.Name, Meth);
         Py_DECREF (Meth);
      end if;
   end Add_Class_Method;

   -----------------------
   -- PyDescr_NewGetSet --
   -----------------------

   function PyDescr_NewGetSet
     (Typ     : PyObject;
      Name    : String;
      Setter  : C_Setter := null;
      Getter  : C_Getter := null;
      Doc     : String   := "";
      Closure : System.Address := System.Null_Address) return Boolean
   is
      function To_Callback is new Standard.Ada.Unchecked_Conversion
        (C_Getter, C_Callback);
      function To_Callback is new Standard.Ada.Unchecked_Conversion
        (C_Setter, C_Callback);

      function Internal
        (Typ : PyObject; Name : chars_ptr;
         Setter, Getter : C_Callback; Doc : chars_ptr;
         Closure : System.Address) return Integer;
      pragma Import (C, Internal, "ada_pydescr_newGetSet");
   begin
      return Internal
        (Typ, New_String (Name), To_Callback (Setter),
         To_Callback (Getter), New_String (Doc), Closure) /= 0;
   end PyDescr_NewGetSet;

   -----------------------
   -- Create_Method_Def --
   -----------------------

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Vargs;
      Doc  : String := "")
      return PyMethodDef is
   begin
      return (Name  => New_String (Name),
              Func  => To_Callback (Func),
              Flags => METH_VARGS,
              Doc   => New_String (Doc));
   end Create_Method_Def;

   -----------------------
   -- Create_Method_Def --
   -----------------------

   function Create_Method_Def
     (Name : String;
      Func : C_Method_Keywords;
      Doc  : String := "")
      return PyMethodDef
   is
      D : chars_ptr := Null_Ptr;
   begin
      if Doc /= "" then
         D := New_String (Doc);
      end if;

      return (Name  => New_String (Name),
              Func  => To_Callback (Func),
              Flags => METH_KEYWORDS or METH_VARGS,
              Doc   => D);
   end Create_Method_Def;

   -------------------
   -- Lookup_Object --
   -------------------

   function Lookup_Object
     (Module : String; Name : String) return PyObject is
   begin
      return Lookup_Object (PyImport_AddModule (Module), Name);
   end Lookup_Object;

   -------------------
   -- Lookup_Object --
   -------------------

   function Lookup_Object
     (Module : PyObject; Name : String) return PyObject
   is
      Dict   : PyObject;
   begin
      if Module /= null then
         Dict := PyModule_GetDict (Module);
         return PyDict_GetItemString (Dict, Name);
      end if;
      return null;
   end Lookup_Object;

   -------------
   -- Py_Main --
   -------------

   function Py_Main return Integer is
      function Internal return Integer;
      pragma Import (C, Internal, "ada_py_main");
   begin
      return Internal;
   end Py_Main;

   ---------------------
   -- PyCObject_Check --
   ---------------------

   function PyCObject_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pycobject_check");
   begin
      return Internal (Obj) = 1;
   end PyCObject_Check;

   --------------------
   -- PyMethod_Check --
   --------------------

   function PyMethod_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pymethod_check");
   begin
      return Internal (Obj) = 1;
   end PyMethod_Check;

   -------------------
   -- Py_IsSubclass --
   -------------------

   function Py_IsSubclass
     (Class : PyObject; Base : PyObject) return Boolean
   is
      function Internal (Class, Base : PyObject) return Integer;
      pragma Import (C, Internal, "ada_is_subclass");
   begin
      return Internal (Class, Base) /= 0;
   end Py_IsSubclass;

   --------------
   -- Type_New --
   --------------

   function Type_New
     (Name     : String;
      Bases    : PyTuple;
      Dict     : PyObject;
      Metatype : PyTypeObject := null) return PyObject
   is
      function Internal
        (Meta  : PyTypeObject;
         Name  : Interfaces.C.Strings.chars_ptr;
         Bases : PyObject;
         Dict  : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_type_new");

      C : chars_ptr := New_String (Name);
      Result : PyObject;

   begin
      Result := Internal (Metatype, C, Bases, Dict);
      Free (C);
      return Result;
   end Type_New;

   ---------
   -- Name --
   ----------

   function Name (Obj : PyTypeObject) return String is
      function Internal (Obj : PyTypeObject) return chars_ptr;
      pragma Import (C, Internal, "ada_tp_name");

   begin
      return Value (Internal (Obj));
   end Name;

   -------------------------
   -- PyObject_IsInstance --
   -------------------------

   function PyObject_IsInstance
     (Inst : PyObject; Cls : PyObject) return Boolean
   is
      function Internal (Inst, Cls : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_IsInstance");
   begin
      return Internal (Inst, Cls) /= 0;
   end PyObject_IsInstance;

   ---------------------
   -- PyObject_IsTrue --
   ---------------------

   function PyObject_IsTrue (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_IsTrue");
      Val : Integer;
   begin
      Val := Internal (Obj);
      if Val = -1 then
         return False;  --  An error
      else
         return Val /= 0;
      end if;
   end PyObject_IsTrue;

end GNATCOLL.Python;
