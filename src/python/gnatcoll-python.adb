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
with Interfaces.C;         use Interfaces.C;

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

   function PyGILState_Ensure return int;
   pragma Import (C, PyGILState_Ensure, "PyGILState_Ensure");
   --  Ensure that the current thread is ready to call the Python
   --  C API, regardless of the current state of Python, or of its
   --  thread lock.  This may be called as many times as desired
   --  by a thread so long as each call is matched with a call to
   --  PyGILState_Release().  In general, other thread-state APIs may
   --  be used between _Ensure() and _Release() calls, so long as the
   --  thread-state is restored to its previous state before the Release().
   --  For example, normal use of the Py_BEGIN_ALLOW_THREADS/
   --  Py_END_ALLOW_THREADS macros are acceptable.
   --
   --  The return value is an opaque "handle" to the thread state when
   --  PyGILState_Ensure() was called, and must be passed to
   --  PyGILState_Release() to ensure Python is left in the same state. Even
   --  though recursive calls are allowed, these handles can *not* be shared -
   --  each unique call to PyGILState_Ensure must save the handle for its
   --  call to PyGILState_Release.
   --
   --  When the function returns, the current thread will hold the GIL.
   --
   --  Failure is a fatal error.

   procedure PyGILState_Release (State : int);
   pragma Import (C, PyGILState_Release, "PyGILState_Release");
   --  Release any resources previously acquired.  After this call, Python's
   --  state will be the same as it was prior to the corresponding
   --  PyGILState_Ensure() call (but generally this state will be unknown to
   --  the caller, hence the use of the GILState API.)
   --
   --  Every call to PyGILState_Ensure must be matched by a call to
   --  PyGILState_Release on the same thread.

   ------------------------
   -- PyRun_SimpleString --
   ------------------------

   function PyRun_SimpleString (Cmd : String) return Boolean is
      function Internal (Cmd : String) return Integer;
      pragma Import (C, Internal, "PyRun_SimpleString");
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Cmd & ASCII.NUL) = 0;
      PyGILState_Release (State);
      return Result;
   end PyRun_SimpleString;

   ------------------------
   -- PyImport_AddModule --
   ------------------------

   function PyImport_AddModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_AddModule");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Module_Name & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
   end PyImport_AddModule;

   ---------------------------
   -- PyImport_ImportModule --
   ---------------------------

   function PyImport_ImportModule (Module_Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PyImport_ImportModule");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Module_Name & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
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
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Str & ASCII.LF, Start, Globals, Locals);
      PyGILState_Release (State);
      return Result;
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Arg, Format & ASCII.NUL, Value1) = 1;
      PyGILState_Release (State);
      return Result;
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Arg, Format & ASCII.NUL, Value1, Value2) = 1;
      PyGILState_Release (State);
      return Result;
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Arg, Format & ASCII.NUL, Value1, Value2, Value3) = 1;
      PyGILState_Release (State);
      return Result;
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4) = 1;
      PyGILState_Release (State);
      return Result;
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal
        (Arg, Format & ASCII.NUL, Value1, Value2, Value3, Value4, Value5) = 1;
      PyGILState_Release (State);
      return Result;
   end PyArg_ParseTuple;

   ----------------------
   -- PyFunction_Check --
   ----------------------

   function PyFunction_Check (Func : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyfunction_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func) = 1;
      PyGILState_Release (State);
      return Result;
   end PyFunction_Check;

   ----------------------
   -- PyCallable_Check --
   ----------------------

   function PyCallable_Check (Func : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyCallable_Check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func) = 1;
      PyGILState_Release (State);
      return Result;
   end PyCallable_Check;

   --------------------
   -- PyString_Check --
   --------------------

   function PyString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pystring_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyString_Check;

   ---------------------
   -- PyUnicode_Check --
   ---------------------

   function PyUnicode_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyunicode_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyUnicode_Check;

   ------------------------
   -- PyBaseString_Check --
   ------------------------

   function PyBaseString_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybasestring_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyBaseString_Check;

   ------------------
   -- PyList_Check --
   ------------------

   function PyList_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pylist_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyList_Check;

   ------------------
   -- PyIter_Check --
   ------------------

   function PyIter_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyiter_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyIter_Check;

   -----------------
   -- PyInt_Check --
   -----------------

   function PyInt_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyint_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyInt_Check;

   -------------------
   -- PyFloat_Check --
   -------------------

   function PyFloat_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyfloat_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
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
      State  : int;
   begin
      State := PyGILState_Ensure;
      if Value then
         Result := PyTrue;
      else
         Result := PyFalse;
      end if;

      Py_INCREF (Result);
      PyGILState_Release (State);
      return Result;
   end PyBool_FromBoolean;

   ------------------
   -- PyBool_Check --
   ------------------

   function PyBool_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybool_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyBool_Check;

   --------------------
   -- PyBool_Is_True --
   --------------------

   function PyBool_Is_True (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pybool_is_true");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyBool_Is_True;

   -------------------
   -- PyTuple_Check --
   -------------------

   function PyTuple_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pytuple_check");
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
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
      State  : constant int := PyGILState_Ensure;
      --  PyGILState_Ensure
      function Low (Str : PyObject) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Low, "ada_PyString_AsString");
      --  Returns a NULL terminated representation of the contents of string.
      --  Result value must be freed.

      C : Interfaces.C.Strings.chars_ptr := Low (Str);
   begin
      if C = Null_Ptr then
         PyGILState_Release (State);
         return "";
      else
         declare
            R : constant String := Value (C);
         begin
            Free (C);
            PyGILState_Release (State);
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
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Str, Str'Length);
      PyGILState_Release (State);
      return Result;
   end PyString_FromString;

   --------------------------
   -- PyUnicode_FromString --
   --------------------------

   function PyUnicode_FromString (Str : String) return PyObject is
      function Internal (Str : String) return PyObject;
      pragma Import (C, Internal, "ada_PyUnicode_FromString");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Str & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
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
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      case Errors is
         when Strict =>
            Result := Internal
              (Unicode, Encoding & ASCII.NUL, "strict" & ASCII.NUL);
         when Ignore =>
            Result := Internal
              (Unicode, Encoding & ASCII.NUL, "ignore" & ASCII.NUL);
         when Replace =>
            Result := Internal
              (Unicode, Encoding & ASCII.NUL, "replace" & ASCII.NUL);
      end case;
      PyGILState_Release (State);
      return Result;
   end PyUnicode_AsEncodedString;

   ----------------------
   -- Unicode_AsString --
   ----------------------

   function Unicode_AsString
     (Str : PyObject; Encoding : String := "utf-8") return String
   is
      State  : constant int := PyGILState_Ensure;
      S : constant PyObject := PyUnicode_AsEncodedString
        (Unicode => Str, Encoding => Encoding, Errors => Replace);
      Result : constant String := PyString_AsString (S);
   begin
      Py_DECREF (S);
      PyGILState_Release (State);
      return Result;
   end Unicode_AsString;

   ---------------------
   -- PySys_SetObject --
   ---------------------

   procedure PySys_SetObject (Name : String; Object : PyObject) is
      procedure Internal (Name : String; Object : PyObject);
      pragma Import (C, Internal, "PySys_SetObject");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Name & ASCII.NUL, Object);
      PyGILState_Release (State);
   end PySys_SetObject;

   ---------------------
   -- PySys_GetObject --
   ---------------------

   function PySys_GetObject (Name : String) return PyObject is
      function Internal (Name : String) return PyObject;
      pragma Import (C, Internal, "PySys_GetObject");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Name & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
   end PySys_GetObject;

   -------------------------
   -- PyObject_CallMethod --
   -------------------------

   function PyObject_CallMethod
     (Object : PyObject; Name : String) return PyObject
   is
      function Internal (Object : PyObject; Name : String) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : PyObject) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_obj");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name & ASCII.NUL, Arg1);
      PyGILState_Release (State);
      return Result;
   end PyObject_CallMethod;

   function PyObject_CallMethod
     (Object : PyObject; Name : String; Arg1 : Integer) return PyObject
   is
      function Internal
        (Object : PyObject; Name : String; Arg : Integer) return PyObject;
      pragma Import (C, Internal, "ada_py_object_callmethod_int");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name & ASCII.NUL, Arg1);
      PyGILState_Release (State);
      return Result;
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
      St     : constant int := PyGILState_Ensure;
      Result : PyCodeObject;
   begin
      Result := Internal (Cmd & ASCII.NUL, Name & ASCII.NUL, State);
      PyGILState_Release (St);
      return Result;
   end Py_CompileString;

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
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Name & ASCII.NUL, Base, Dict);
      PyGILState_Release (State);
      return Result;
   end PyErr_NewException;

   ---------------------
   -- PyErr_SetString --
   ---------------------

   procedure PyErr_SetString (Except : PyObject; Msg : String) is
      procedure Internal (Except : PyObject; Msg : String);
      pragma Import (C, Internal, "PyErr_SetString");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Except, Msg & ASCII.NUL);
      PyGILState_Release (State);
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
      Result : Boolean;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Boolean'Val (Internal (Obj, Attr_Name & ASCII.NUL));
      PyGILState_Release (State);
      return Result;
   end PyObject_HasAttrString;

   ----------------------------
   -- PyObject_SetAttrString --
   ----------------------------

   procedure PyObject_SetAttrString
     (Obj : PyObject; Attr_Name : String; Value : PyObject)
   is
      procedure Internal (Obj : PyObject; Name : String; Val : PyObject);
      pragma Import (C, Internal, "PyObject_SetAttrString");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Obj, Attr_Name & ASCII.NUL, Value);
      PyGILState_Release (State);
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
      Result : Integer;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj, Attr_Name & ASCII.NUL, Value);
      PyGILState_Release (State);
      return Result;
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
      State  : int;

   begin
      State := PyGILState_Ensure;
      if Internal (Dict, P'Address, Key'Address, Value'Address) = 0 then
         Pos := -1;
      else
         Pos := Integer (P);
      end if;
      PyGILState_Release (State);
   end PyDict_Next;

   --------------------
   -- Print_Refcount --
   --------------------

   procedure Print_Refcount (Obj : PyObject; Msg : String) is
      procedure Internal (Obj : PyObject; Msg : String);
      pragma Import (C, Internal, "ada_py_print_refcount");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Obj, Msg & ASCII.NUL);
      PyGILState_Release (State);
   end Print_Refcount;

   ------------------------
   -- PyFile_WriteString --
   ------------------------

   function PyFile_WriteString
     (Text : String; File : PyObject) return Boolean
   is
      function Internal (Text : String; File : PyObject) return Integer;
      pragma Import (C, Internal, "PyFile_WriteString");
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Text & ASCII.NUL, File) /= 0;
      PyGILState_Release (State);
      return Result;
   end PyFile_WriteString;

   -----------------------
   -- PyFile_FromString --
   -----------------------

   function PyFile_FromString (File_Name, Mode : String) return PyObject is
      function Internal (N, M : String) return PyObject;
      pragma Import (C, Internal, "PyFile_FromString");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (File_Name & ASCII.NUL, Mode & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
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
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      if Methods /= No_MethodDef_Array then
         --  ??? Memory is never freed, but Python is not supposed to be killed
         --  before the end of the application
         M := new PyMethodDef_Array'(Methods & No_Method_Def);
         Result := Internal
           (Module_Name & ASCII.NUL, M.all'Address,
            Doc & ASCII.NUL);

      else
         Result := Internal
           (Module_Name & ASCII.NUL, System.Null_Address,
            Doc & ASCII.NUL);
      end if;
      PyGILState_Release (State);
      return Result;
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
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Module_Name & ASCII.NUL);
      PyGILState_Release (State);
      return Result;
   end PyModule_New;

   ----------------------
   -- PyModule_Getname --
   ----------------------

   function PyModule_Getname (Module : PyObject) return String is
      function Internal (M : PyObject) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "PyModule_GetName");
      State  : int;
   begin
      State := PyGILState_Ensure;
      declare
         Result : constant String := Value (Internal (Module));
      begin
         PyGILState_Release (State);
         return Result;
      end;
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
      State  : int;
   begin
      State := PyGILState_Ensure;
      Add_Method (new PyMethodDef'(Func), Self, Class, Module);
      PyGILState_Release (State);
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
      State  : int;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_STATIC;

      C_Func := PyCFunction_New
        (Def, Self, PyString_FromString (PyModule_Getname (Module)));
      if C_Func /= null then
         --  ??? Likely not needed for python3
         State := PyGILState_Ensure;
         Static := PyStaticMethod_New (C_Func);
         PyGILState_Release (State);
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
      State  : int;
      pragma Unreferenced (Result);
   begin
      Def.Flags := Def.Flags or METH_CLASS;

      C_Func := PyCFunction_New
        (Def, null, PyString_FromString (PyModule_Getname (Module)));
      if C_Func /= null then
         State := PyGILState_Ensure;
         Meth := PyClassMethod_New (C_Func);
         PyGILState_Release (State);
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
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal
        (Typ, New_String (Name), To_Callback (Setter),
         To_Callback (Getter), New_String (Doc), Closure) /= 0;
      PyGILState_Release (State);
      return Result;
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
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyCObject_Check;

   --------------------
   -- PyMethod_Check --
   --------------------

   function PyMethod_Check (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pymethod_check");
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj) = 1;
      PyGILState_Release (State);
      return Result;
   end PyMethod_Check;

   -------------------
   -- Py_IsSubclass --
   -------------------

   function Py_IsSubclass
     (Class : PyObject; Base : PyObject) return Boolean
   is
      function Internal (Class, Base : PyObject) return Integer;
      pragma Import (C, Internal, "ada_is_subclass");
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Class, Base) /= 0;
      PyGILState_Release (State);
      return Result;
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
      State  : int;
      Result : PyObject;

   begin
      State := PyGILState_Ensure;
      Result := Internal (Metatype, C, Bases, Dict);
      PyGILState_Release (State);
      Free (C);
      return Result;
   end Type_New;

   -------------------------
   -- PyObject_IsInstance --
   -------------------------

   function PyObject_IsInstance
     (Inst : PyObject; Cls : PyObject) return Boolean
   is
      function Internal (Inst, Cls : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_IsInstance");
      State  : int;
      Result : Boolean;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Inst, Cls) /= 0;
      PyGILState_Release (State);
      return Result;
   end PyObject_IsInstance;

   ---------------------
   -- PyObject_IsTrue --
   ---------------------

   function PyObject_IsTrue (Obj : PyObject) return Boolean is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_IsTrue");
      State  : int;
      Val : Integer;
   begin
      State := PyGILState_Ensure;
      Val := Internal (Obj);
      PyGILState_Release (State);
      if Val = -1 then
         return False;  --  An error
      else
         return Val /= 0;
      end if;
   end PyObject_IsTrue;

   -------------------
   -- PyObject_Call --
   -------------------

   function PyObject_Call
     (Object : PyObject; Args : PyObject; Kw : PyObject) return PyObject
   is
      function Internal
        (Object : PyObject; Args : PyObject; Kw : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_Call");

      State : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Args, Kw);
      PyGILState_Release (State);

      return Result;
   end PyObject_Call;

   -----------------
   -- PyErr_Clear --
   -----------------

   procedure PyErr_Clear is
      procedure Internal;
      pragma Import (C, Internal, "PyErr_Clear");

      State : int;
   begin
      State := PyGILState_Ensure;
      Internal;
      PyGILState_Release (State);
   end PyErr_Clear;

   ---------------
   -- Py_INCREF --
   ---------------

   procedure Py_INCREF (Obj : PyObject) is
      State  : int;
      procedure Internal (Obj : PyObject);
      pragma Import (C, Internal, "ada_py_incref");
   begin
      State := PyGILState_Ensure;
      Internal (Obj);
      PyGILState_Release (State);
   end Py_INCREF;

   ---------------
   -- Py_DECREF --
   ---------------

   procedure Py_DECREF (Obj : PyObject) is
      State  : int;
      procedure Internal (Obj : PyObject);
      pragma Import (C, Internal, "ada_py_decref");
   begin
      State := PyGILState_Ensure;
      Internal (Obj);
      PyGILState_Release (State);
   end Py_DECREF;

   ----------------
   -- Py_XINCREF --
   ----------------

   procedure Py_XINCREF (Obj : PyObject) is
      State  : int;
      procedure Internal (Obj : PyObject);
      pragma Import (C, Internal, "ada_py_xincref");
   begin
      State := PyGILState_Ensure;
      Internal (Obj);
      PyGILState_Release (State);
   end Py_XINCREF;

   ----------------
   -- Py_XDECREF --
   ----------------

   procedure Py_XDECREF (Obj : PyObject) is
      State  : int;
      procedure Internal (Obj : PyObject);
      pragma Import (C, Internal, "ada_py_xdecref");
   begin
      State := PyGILState_Ensure;
      Internal (Obj);
      PyGILState_Release (State);
   end Py_XDECREF;

   ---------------------
   -- PyEval_EvalCode --
   ---------------------

   function PyEval_EvalCode
     (Code    : PyCodeObject;
      Globals : PyObject;
      Locals  : PyObject) return PyObject
   is
      function Internal
        (Code    : PyCodeObject;
         Globals : PyObject;
         Locals  : PyObject) return PyObject;
      pragma Import (C, Internal, "PyEval_EvalCode");

      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Code, Globals, Locals);
      PyGILState_Release (State);
      return Result;
   end PyEval_EvalCode;

   ----------------
   -- PyDict_New --
   ----------------

   function PyDict_New return PyDictObject is
      function Internal return PyDictObject;
      pragma Import (C, Internal, "PyDict_New");
      State  : int;
      Result : PyDictObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal;
      PyGILState_Release (State);
      return Result;
   end PyDict_New;

   -----------------------
   -- PyEval_SetProfile --
   -----------------------

   procedure PyEval_SetProfile (Proc : Py_Trace_Func; User_Arg : PyObject) is
      procedure Internal (Proc : Py_Trace_Func; User_Arg : PyObject);
      pragma Import (C, Internal, "PyEval_SetProfile");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Proc, User_Arg);
      PyGILState_Release (State);
   end PyEval_SetProfile;

   -----------------------
   -- PyEval_SetTrace --
   -----------------------

   procedure PyEval_SetTrace (Proc : Py_Trace_Func; User_Arg : PyObject) is
      procedure Internal (Proc : Py_Trace_Func; User_Arg : PyObject);
      pragma Import (C, Internal, "PyEval_SetTrace");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Proc, User_Arg);
      PyGILState_Release (State);
   end PyEval_SetTrace;

   ----------------------
   -- PyModule_GetDict --
   ----------------------

   function PyModule_GetDict (Module : PyObject) return PyObject is
      function Internal (Module : PyObject) return PyObject;
      pragma Import (C, Internal, "PyModule_GetDict");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Module);
      PyGILState_Release (State);
      return Result;
   end PyModule_GetDict;

   -----------------
   -- PyErr_Print --
   -----------------

   procedure PyErr_Print is
      procedure Internal;
      pragma Import (C, Internal, "PyErr_Print");
      State  : constant int := PyGILState_Ensure;
   begin
      Internal;
      PyGILState_Release (State);
   end PyErr_Print;

   ------------------------
   -- PyErr_SetInterrupt --
   ------------------------

   procedure PyErr_SetInterrupt is
      procedure Internal;
      pragma Import (C, Internal, "PyErr_SetInterrupt");
      State  : constant int := PyGILState_Ensure;
   begin
      Internal;
      PyGILState_Release (State);
   end PyErr_SetInterrupt;

   ------------------
   -- PyObject_Str --
   ------------------

   function PyObject_Str (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_Str");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyObject_Str;

   -------------------
   -- PyObject_Repr --
   -------------------

   function PyObject_Repr (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_Repr");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyObject_Repr;

   -------------------
   -- PyObject_Dir --
   -------------------

   function PyObject_Dir (Object : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_Dir");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object);
      PyGILState_Release (State);
      return Result;
   end PyObject_Dir;

   -----------------------
   -- PyEval_EvalCodeEx --
   -----------------------

   function PyEval_EvalCodeEx
     (Code     : PyCodeObject;
      Globals  : PyObject;
      Locals   : PyObject;
      Args     : PyTuple      := null;
      Kwds     : PyDictObject := null;
      Defaults : PyTuple      := null;
      Closure  : PyObject     := null) return PyObject
   is
      function Internal
        (Code     : PyCodeObject;
         Globals  : PyObject;
         Locals   : PyObject;
         Args     : PyTuple      := null;
         Kwds     : PyDictObject := null;
         Defaults : PyTuple      := null;
         Closure  : PyObject     := null) return PyObject;
      pragma Import (C, Internal, "ada_PyEval_EvalCodeEx");

      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal
        (Code, Globals, Locals, Args, Kwds, Defaults, Closure);
      PyGILState_Release (State);
      return Result;
   end PyEval_EvalCodeEx;

   -----------------
   -- PyTuple_New --
   -----------------

   function PyTuple_New (Size : Integer) return PyObject is
      function Internal (Size : Integer) return PyObject;
      pragma Import (C, Internal, "PyTuple_New");

      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Size);
      PyGILState_Release (State);
      return Result;
   end PyTuple_New;

   ---------------------
   -- PyTuple_GetItem --
   ---------------------

   function PyTuple_GetItem
     (Tuple : PyTuple; Index : Integer) return PyObject
   is
      function Internal (Tuple : PyTuple; Index : Integer) return PyObject;
      pragma Import (C, Internal, "PyTuple_GetItem");
      Result : PyObject;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Tuple, Index);
      PyGILState_Release (State);
      return Result;
   end PyTuple_GetItem;

   ---------------------
   -- PyTuple_SetItem --
   ---------------------

   procedure PyTuple_SetItem
     (Tuple : PyTuple; Index : Integer; Value : PyObject)
   is
      procedure Internal (Tuple : PyTuple; Index : Integer; Value : PyObject);
      pragma Import (C, Internal, "PyTuple_SetItem");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (Tuple, Index, Value);
      PyGILState_Release (State);
   end PyTuple_SetItem;

   -------------
   -- Py_None --
   -------------

   function Py_None return PyObject is
      function Internal return PyObject;
      pragma Import (C, Internal, "ada_py_none");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal;
      PyGILState_Release (State);
      return Result;
   end Py_None;

   -----------------
   -- PyErr_Fetch --
   -----------------

   procedure PyErr_Fetch
     (EType      : out PyObject;
      Occurrence : out PyObject;
      Traceback  : out PyObject)
   is
      procedure Internal
        (EType      : out PyObject;
         Occurrence : out PyObject;
         Traceback  : out PyObject);
      pragma Import (C, Internal, "PyErr_Fetch");

      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (EType, Occurrence, Traceback);
      PyGILState_Release (State);
   end PyErr_Fetch;

   ------------------
   -- PyTuple_Size --
   ------------------

   function PyTuple_Size (Tuple : PyTuple) return Integer is
      function Internal (Tuple : PyTuple) return Integer;
      pragma Import (C, Internal, "PyTuple_Size");
      Result : Integer;
      State  : int;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Tuple);
      PyGILState_Release (State);
      return Result;
   end PyTuple_Size;

   --------------------
   -- PyInt_FromLong --
   --------------------

   function PyInt_FromLong (Value : Interfaces.C.long) return PyObject is

      function Internal (Value : Interfaces.C.long) return PyObject;
      pragma Import (C, Internal, "PyInt_FromLong");

      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Value);
      PyGILState_Release (State);
      return Result;
   end PyInt_FromLong;

   ------------------
   -- PyInt_AsLong --
   ------------------

   function PyInt_AsLong (Int : PyObject) return Interfaces.C.long is
      function Internal (Int : PyObject) return Interfaces.C.long;
      pragma Import (C, Internal, "PyInt_AsLong");
      State  : Interfaces.C.int;
      Result : Interfaces.C.long;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Int);
      PyGILState_Release (State);
      return Result;
   end PyInt_AsLong;

   ----------------------
   -- PyFloat_AsDouble --
   ----------------------

   function PyFloat_AsDouble (Float : PyObject) return Interfaces.C.double is
      function Internal (Float : PyObject) return Interfaces.C.double;
      pragma Import (C, Internal, "PyFloat_AsDouble");
      State  : int;
      Result : Interfaces.C.double;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Float);
      PyGILState_Release (State);
      return Result;
   end PyFloat_AsDouble;

   ------------------
   -- PyInt_GetMax --
   ------------------

   function PyInt_GetMax return Interfaces.C.long is
      function Internal return Interfaces.C.long;
      pragma Import (C, Internal, "PyInt_GetMax");
      State  : int;
      Result : Interfaces.C.long;
   begin
      State := PyGILState_Ensure;
      Result := Internal;
      PyGILState_Release (State);
      return Result;
   end PyInt_GetMax;

   --------------------
   -- PyErr_Occurred --
   --------------------

   function PyErr_Occurred return PyObject is
      function Internal return PyObject;
      pragma Import (C, Internal, "PyErr_Occurred");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal;
      PyGILState_Release (State);
      return Result;
   end PyErr_Occurred;

   ----------------
   -- PyList_New --
   ----------------

   function PyList_New (Size : Integer := 0) return PyObject is
      function Internal (Size : Integer) return PyObject;
      pragma Import (C, Internal, "PyList_New");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Size);
      PyGILState_Release (State);
      return Result;
   end PyList_New;

   -------------------
   -- PyList_Append --
   -------------------

   function PyList_Append (List : PyObject; Obj : PyObject) return Integer is
      function Internal (List : PyObject; Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyList_Append");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (List, Obj);
      PyGILState_Release (State);
      return Result;
   end PyList_Append;

   -----------------------
   -- PyErr_BadArgument --
   -----------------------

   procedure PyErr_BadArgument is
      procedure Internal;
      pragma Import (C, Internal, "PyErr_BadArgument");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal;
      PyGILState_Release (State);
   end PyErr_BadArgument;

   ------------------------------
   -- PyErr_NormalizeException --
   ------------------------------

   procedure PyErr_NormalizeException
     (EType      : in out PyObject;
      Occurrence : in out PyObject;
      Traceback  : in out PyObject)
   is

      procedure Internal
        (EType      : in out PyObject;
         Occurrence : in out PyObject;
         Traceback  : in out PyObject);
      pragma Import (C, Internal, "PyErr_NormalizeException");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (EType, Occurrence, Traceback);
      PyGILState_Release (State);
   end PyErr_NormalizeException;

   -------------------
   -- PyErr_Restore --
   -------------------

   procedure PyErr_Restore
     (EType      : PyObject;
      Occurrence : PyObject;
      Traceback  : PyObject)
   is
      procedure Internal
        (EType      : PyObject;
         Occurrence : PyObject;
         Traceback  : PyObject);
      pragma Import (C, Internal, "PyErr_Restore");
      State  : int;
   begin
      State := PyGILState_Ensure;
      Internal (EType, Occurrence, Traceback);
      PyGILState_Release (State);
   end PyErr_Restore;

   -----------------
   -- PyDict_Size --
   -----------------

   function PyDict_Size (Dict : PyObject) return Integer is

      function Internal (Dict : PyObject) return Integer;
      pragma Import (C, Internal, "PyDict_Size");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Dict);
      PyGILState_Release (State);
      return Result;
   end PyDict_Size;

   --------------------
   -- PyList_GetItem --
   --------------------

   function PyList_GetItem
     (List : PyObject; Index : Integer) return PyObject
   is
      function Internal (List : PyObject; Index : Integer) return PyObject;
      pragma Import (C, Internal, "PyList_GetItem");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (List, Index);
      PyGILState_Release (State);
      return Result;
   end PyList_GetItem;

   -----------------
   -- PyList_Size --
   -----------------

   function PyList_Size (List : PyObject) return Integer is

      function Internal (List : PyObject) return Integer;
      pragma Import (C, Internal, "PyList_Size");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (List);
      PyGILState_Release (State);
      return Result;
   end PyList_Size;

   --------------------
   -- PyDict_SetItem --
   --------------------

   function PyDict_SetItem
     (Dict  : PyDictObject;
      Key   : PyObject;
      Value : PyObject) return Integer
   is
      function Internal
        (Dict  : PyDictObject;
         Key   : PyObject;
         Value : PyObject) return Integer;
      pragma Import (C, Internal, "PyDict_SetItem");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Dict, Key, Value);
      PyGILState_Release (State);
      return Result;
   end PyDict_SetItem;

   --------------------
   -- PyDict_GetItem --
   --------------------

   function PyDict_GetItem
     (Dict : PyDictObject; Key : PyObject) return PyObject
   is
      function Internal
        (Dict : PyDictObject; Key : PyObject) return PyObject;
      pragma Import (C, Internal, "PyDict_GetItem");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Dict, Key);
      PyGILState_Release (State);
      return Result;
   end PyDict_GetItem;

   ------------------
   -- Get_Refcount --
   ------------------

   function Get_Refcount (Obj : PyObject) return Integer is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "ada_pyget_refcount");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end Get_Refcount;

   -------------------------
   -- PyFunction_Get_Code --
   -------------------------

   function PyFunction_Get_Code (Func : PyObject) return PyCodeObject is
      function Internal (Func : PyObject) return PyCodeObject;
      pragma Import (C, Internal, "ada_pyfunction_get_code");
      State  : int;
      Result : PyCodeObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func);
      PyGILState_Release (State);
      return Result;
   end PyFunction_Get_Code;

   ----------------------------
   -- PyFunction_Get_Globals --
   ----------------------------

   function PyFunction_Get_Globals (Func : PyObject) return PyObject is
      function Internal (Func : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_pyfunction_get_globals");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func);
      PyGILState_Release (State);
      return Result;
   end PyFunction_Get_Globals;

   ----------------------------
   -- PyFunction_Get_Closure --
   ----------------------------

   function PyFunction_Get_Closure (Func : PyObject) return PyObject is
      function Internal (Func : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_pyfunction_get_closure");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func);
      PyGILState_Release (State);
      return Result;
   end PyFunction_Get_Closure;

   -----------------------------
   -- PyFunction_Get_Defaults --
   -----------------------------

   function PyFunction_Get_Defaults (Func : PyObject) return PyObject is
      function Internal (Func : PyObject) return PyObject;
      pragma Import (C, Internal, "ada_pyfunction_get_defaults");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Func);
      PyGILState_Release (State);
      return Result;
   end PyFunction_Get_Defaults;

   -------------------
   -- GetTypeObject --
   -------------------

   function GetTypeObject (Obj : PyObject) return PyTypeObject is
      function Internal (Obj : PyObject) return PyTypeObject;
      pragma Import (C, Internal, "ada_gettypeobject");
      State  : int;
      Result : PyTypeObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end GetTypeObject;

   ---------------------------
   -- PyCObject_FromVoidPtr --
   ---------------------------

   function PyCObject_FromVoidPtr
     (Obj   : System.Address;
      Destr : PyCObject_Destructor := null)
      return PyObject
   is
      function Internal
        (Obj   : System.Address;
         Destr : PyCObject_Destructor := null)
         return PyObject;
      pragma Import (C, Internal, "PyCObject_FromVoidPtr");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj, Destr);
      PyGILState_Release (State);
      return Result;
   end PyCObject_FromVoidPtr;

   ----------------------------------
   -- PyCObject_FromVoidPtrAndDesc --
   ----------------------------------

   function PyCObject_FromVoidPtrAndDesc
     (Obj   : System.Address;
      Desc  : System.Address;
      Destr : PyCObject_Destructor2 := null)
      return PyObject
   is
      function Internal
        (Obj   : System.Address;
         Desc  : System.Address;
         Destr : PyCObject_Destructor2 := null)
      return PyObject;
      pragma Import (C, Internal, "PyCObject_FromVoidPtrAndDesc");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj, Desc, Destr);
      PyGILState_Release (State);
      return Result;
   end PyCObject_FromVoidPtrAndDesc;

   -------------------------
   -- PyCObject_AsVoidPtr --
   -------------------------

   function PyCObject_AsVoidPtr (Self : PyObject) return System.Address is
      function Internal (Self : PyObject) return System.Address;
      pragma Import (C, Internal, "PyCObject_AsVoidPtr");
      State  : int;
      Result : System.Address;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Self);
      PyGILState_Release (State);
      return Result;
   end PyCObject_AsVoidPtr;

   -----------------------
   -- PyCObject_GetDesc --
   -----------------------

   function PyCObject_GetDesc (Self : PyObject) return System.Address is
      function Internal (Self : PyObject) return System.Address;
      pragma Import (C, Internal, "PyCObject_GetDesc");
      State  : int;
      Result : System.Address;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Self);
      PyGILState_Release (State);
      return Result;
   end PyCObject_GetDesc;

   -----------------------
   -- PyMethod_Function --
   -----------------------

   function PyMethod_Function (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyMethod_Function");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyMethod_Function;

   -------------------
   -- PyMethod_Self --
   -------------------

   function PyMethod_Self (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyMethod_Self");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyMethod_Self;

   -------------------------
   -- PyObject_CallObject --
   -------------------------

   function PyObject_CallObject
     (Object : PyObject; Args : PyObject) return PyObject is

      function Internal
        (Object : PyObject; Args : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_CallObject");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Args);
      PyGILState_Release (State);
      return Result;
   end PyObject_CallObject;

   ----------------------------
   -- PyObject_SetAttrString --
   ----------------------------

   function PyObject_SetAttrString
     (Object : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Attr   : PyObject) return Integer is

      function Internal
        (Object : PyObject;
         Name   : Interfaces.C.Strings.chars_ptr;
         Attr   : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_SetAttrString");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name, Attr);
      PyGILState_Release (State);
      return Result;
   end PyObject_SetAttrString;

   -----------------------------
   -- PyObject_GenericSetAttr --
   -----------------------------

   function PyObject_GenericSetAttr
     (Object : PyObject;
      Name   : PyObject;
      Attr   : PyObject) return Integer
   is
      function Internal
        (Object : PyObject;
         Name   : PyObject;
         Attr   : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_GenericSetAttr");

      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name, Attr);
      PyGILState_Release (State);
      return Result;
   end PyObject_GenericSetAttr;

   ----------------------------
   -- PyObject_GetAttrString --
   ----------------------------

   function PyObject_GetAttrString
     (Object : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr) return PyObject
   is
      function Internal
        (Object : PyObject;
         Name   : Interfaces.C.Strings.chars_ptr) return PyObject;
      pragma Import (C, Internal, "PyObject_GetAttrString");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Object, Name);
      PyGILState_Release (State);
      return Result;
   end PyObject_GetAttrString;

   -----------------------
   -- PyEval_GetGlobals --
   -----------------------

   function PyEval_GetGlobals return PyObject is
      function Internal return PyObject;
      pragma Import (C, Internal, "PyEval_GetGlobals");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal;
      PyGILState_Release (State);
      return Result;
   end PyEval_GetGlobals;

   ------------------------
   -- PyFloat_FromDouble --
   ------------------------

   function PyFloat_FromDouble
     (Value : Interfaces.C.double) return PyObject
   is
      function Internal (Value : Interfaces.C.double) return PyObject;
      pragma Import (C, Internal, "PyFloat_FromDouble");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Value);
      PyGILState_Release (State);
      return Result;
   end PyFloat_FromDouble;

   ----------------------
   -- PyObject_GetIter --
   ----------------------

   function PyObject_GetIter (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_GetIter");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyObject_GetIter;

   -------------------
   -- PyObject_Size --
   -------------------

   function PyObject_Size (Obj : PyObject) return Integer is
      function Internal (Obj : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_Size");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyObject_Size;

   ----------------------
   -- PyObject_GetItem --
   ----------------------

   function PyObject_GetItem (Obj, Key : PyObject) return PyObject is
      function Internal (Obj, Key : PyObject) return PyObject;
      pragma Import (C, Internal, "PyObject_GetItem");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj, Key);
      PyGILState_Release (State);
      return Result;
   end PyObject_GetItem;

   ----------------------
   -- PyObject_SetItem --
   ----------------------

   function PyObject_SetItem (Obj, Key, Value : PyObject) return Integer is
      function Internal (Obj, Key, Value : PyObject) return Integer;
      pragma Import (C, Internal, "PyObject_SetItem");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj, Key, Value);
      PyGILState_Release (State);
      return Result;
   end PyObject_SetItem;

   -----------------
   -- PyIter_Next --
   -----------------

   function PyIter_Next (Obj : PyObject) return PyObject is
      function Internal (Obj : PyObject) return PyObject;
      pragma Import (C, Internal, "PyIter_Next");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Obj);
      PyGILState_Release (State);
      return Result;
   end PyIter_Next;

   ---------------------
   -- PyImport_Import --
   ---------------------

   function PyImport_Import (Name : PyObject) return PyObject is
      function Internal (Name : PyObject) return PyObject;
      pragma Import (C, Internal, "PyImport_Import");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Name);
      PyGILState_Release (State);
      return Result;
   end PyImport_Import;

   ------------------------
   -- PyModule_AddObject --
   ------------------------

   function PyModule_AddObject
     (Module : PyObject;
      Name   : Interfaces.C.Strings.chars_ptr;
      Object : PyObject) return Integer
   is
      function Internal
        (Module : PyObject;
         Name   : Interfaces.C.Strings.chars_ptr;
         Object : PyObject) return Integer;
      pragma Import (C, Internal, "PyModule_AddObject");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Module, Name, Object);
      PyGILState_Release (State);
      return Result;
   end PyModule_AddObject;

   --------------------------
   -- PyDict_SetItemString --
   --------------------------

   function PyDict_SetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr;
      Obj  : PyObject) return Integer
   is
      function Internal
        (Dict : PyDictObject;
         Key  : Interfaces.C.Strings.chars_ptr;
         Obj  : PyObject) return Integer;
      pragma Import (C, Internal, "PyDict_SetItemString");
      State  : int;
      Result : Integer;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Dict, Key, Obj);
      PyGILState_Release (State);
      return Result;
   end PyDict_SetItemString;

   --------------------------
   -- PyDict_GetItemString --
   --------------------------

   function PyDict_GetItemString
     (Dict : PyDictObject;
      Key  : Interfaces.C.Strings.chars_ptr) return PyObject
   is
      function Internal
        (Dict : PyDictObject;
         Key  : Interfaces.C.Strings.chars_ptr) return PyObject;
      pragma Import (C, Internal, "PyDict_GetItemString");
      State  : int;
      Result : PyObject;
   begin
      State := PyGILState_Ensure;
      Result := Internal (Dict, Key);
      PyGILState_Release (State);
      return Result;
   end PyDict_GetItemString;

end GNATCOLL.Python;
