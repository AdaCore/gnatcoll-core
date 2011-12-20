/*----------------------------------------------------------------------------
--                          G N A T C O L L                                 --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

/* Force a value for the macro. It will only work for gcc, but otherwise
 * we cannot use the mingwin python with gcc on Windows*/
#define PY_LONG_LONG long long
#include <Python.h>
#include <compile.h>  /* PyCodeObject definition in older versions*/

#undef DEBUG
/* #define DEBUG */

PyObject *
ada_Py_InitModule4
  (char *name, PyMethodDef *methods,
   char *doc, PyObject *self,
   int apiver)
{
  return Py_InitModule4 (name, methods, doc, self, apiver);
}

PyObject *
ada_pycfunction_newex (PyMethodDef *ml, PyObject *self, PyObject *module)
{
  PyObject *method = PyCFunction_New (ml, self);

#if (PY_MAJOR_VERSION > 2 \
     || (PY_MAJOR_VERSION == 2 \
	 && (PY_MINOR_VERSION > 3 \
	     || (PY_MINOR_VERSION == 3 \
		 && PY_MICRO_VERSION >= 3))))
  ((PyCFunctionObject*)method)->m_module = module;
  Py_XINCREF (module);
#endif
  return method;
}

int
ada_pyget_refcount (PyObject* obj)
{
   return obj->ob_refcnt;
}

char*
ada_py_refcount_msg (PyObject* obj)
{
   static char msg[200];
   if (obj) {
      snprintf (msg, 199, "%p (%s, rc=%d)",
                obj, obj->ob_type->tp_name, obj->ob_refcnt);
   } else {
      msg[0] = '\0';
   }
   return msg;
}

void
ada_py_print_refcount (PyObject* obj, char* msg)
{
  if (obj)
    printf ("DEBUG %s %s\n", msg, ada_py_refcount_msg (obj));
}

void
ada_py_incref (PyObject* obj)
{
  Py_INCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after incref");
#endif
}

void
ada_py_decref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before decref");
#endif
  Py_DECREF (obj);
}

void
ada_py_xincref (PyObject* obj)
{
  Py_XINCREF (obj);
#ifdef DEBUG
  ada_py_print_refcount (obj, "after xincref");
#endif
}

void
ada_py_xdecref (PyObject* obj)
{
#ifdef DEBUG
  ada_py_print_refcount (obj, "before xdecref");
#endif
  Py_XDECREF (obj);
}

int
ada_pybasestring_check (PyObject* obj)
{
  return PyString_Check (obj) || PyUnicode_Check (obj);
}

int
ada_pystring_check (PyObject* obj)
{
  return PyString_Check (obj);
}

PyObject* ada_PyUnicode_AsEncodedString
  (PyObject *unicode, const char *encoding, const char *errors)
{
#ifdef Py_UNICODE_WIDE
  return PyUnicodeUCS4_AsEncodedString (unicode, encoding, errors);
#else
  return PyUnicodeUCS2_AsEncodedString (unicode, encoding, errors);
#endif
}

PyObject* ada_PyUnicode_FromString (const char *u)
{
#if PY_VERSION_HEX >= 0x02060000
#ifdef Py_UNICODE_WIDE
  return PyUnicodeUCS4_FromString (u);
#else
  return PyUnicodeUCS2_FromString (u);
#endif
#else
  /* Not available in this version */
  return 0;
#endif
}

int
ada_pyunicode_check (PyObject* obj)
{
  return PyUnicode_Check (obj);
}

int
ada_pyint_check (PyObject* obj)
{
  return PyInt_Check (obj);
}

int
ada_pyfloat_check (PyObject* obj)
{
  return PyFloat_Check (obj);
}

int
ada_pybool_check (PyObject* obj)
{
#ifdef PyBool_Check
  return PyBool_Check (obj);
#else
  return 0;
#endif
}

int
ada_pybool_is_true (PyObject* obj)
{
  return PyObject_IsTrue (obj);
}

int
ada_pyfunction_check (PyObject* obj)
{
  return PyFunction_Check (obj);
}

PyObject*
ada_pyfunction_get_globals (PyObject* obj)
{
  return PyFunction_GET_GLOBALS (obj);
}

PyObject*
ada_pyfunction_get_code (PyObject* obj)
{
  return PyFunction_GET_CODE (obj);
}

PyObject*
ada_pyfunction_get_closure (PyObject* obj)
{
  return PyFunction_GET_CLOSURE (obj);
}

PyObject*
ada_pyfunction_get_defaults (PyObject* obj)
{
  return PyFunction_GET_DEFAULTS (obj);
}

PyObject* ada_PyEval_EvalCodeEx
  (PyCodeObject *co,
   PyObject *globals,
   PyObject *locals,
   PyObject *args,
   PyObject *kwds,
   PyObject *defs,
   PyObject *closure)
{
   /* Code copied from funcobject.c::function_call() */

  PyObject **k, **d;
  PyObject* result;
  int nk, nd;

  if (defs != NULL && PyTuple_Check(defs)) {
     d = &PyTuple_GET_ITEM((PyTupleObject *)defs, 0);
     nd = PyTuple_Size(defs);
  } else {
     d = NULL;
     nd = 0;
  }

  if (kwds != NULL && PyDict_Check(kwds)) {
     int i = 0;
#if PY_MAJOR_VERSION > 2 || (PY_MAJOR_VERSION==2 && PY_MINOR_VERSION>=5)
     Py_ssize_t pos = 0;
#else
     int pos = 0;
#endif
     nk = PyDict_Size(kwds);
     k  = PyMem_NEW(PyObject *, 2*nk);
     if (k == NULL) {
        PyErr_NoMemory();
        return NULL;
     }
     while (PyDict_Next(kwds, &pos, &k[i], &k[i+1]))
        i += 2;
      nk = i/2;
      /* XXX This is broken if the caller deletes dict items! */
  } else {
     k = NULL;
     nk = 0;
  }

  result = (PyObject*) PyEval_EvalCodeEx
    (co, globals, locals,
     &PyTuple_GET_ITEM (args, 0) /* args */,
     PyTuple_Size (args) /* argcount */,
     k /* kws */, nk /* kwcount */, d /* defs */, nd /* defcount */, closure);

  if (k != NULL) {
    PyMem_DEL (k);
  }

  return result;
}

int
ada_pycobject_check (PyObject* obj)
{
  return PyCObject_Check (obj);
}

int
ada_pytuple_check (PyObject* obj)
{
  return PyTuple_Check (obj);
}

int
ada_pylist_check (PyObject* obj)
{
  return PyList_Check (obj);
}

int
ada_pyiter_check (PyObject* obj)
{
  return PyIter_Check (obj);
}

int
ada_pyinstance_check (PyObject* obj)
{
  return PyInstance_Check (obj);
}

int
ada_pymethod_check (PyObject* obj)
{
  return PyMethod_Check (obj);
}

PyTypeObject*
ada_gettypeobject (PyObject* obj)
{
  return (PyTypeObject*)(obj->ob_type);
}

int
ada_python_api_version ()
{
  return PYTHON_API_VERSION;
}

PyObject* ada_py_none ()
{
  return Py_None;
}

PyObject* ada_py_false()
{
  return Py_False;
}

PyObject*
ada_py_true()
{
  return Py_True;
}

PyObject*
ada_pyclass_name(PyObject* obj)
{
  if (PyClass_Check (obj)) {
      return ((PyClassObject*)obj)->cl_name;
  } else {
     /* Derives from object, not a real class */
     return PyObject_GetAttrString (obj, "__name__");
  }
}

int
ada_pyclass_is_subclass (PyObject* class, PyObject* base)
{
  if (!class || !base) {
    return -1;
  } else if (PyClass_Check (class)) {
    return PyClass_IsSubclass (class, base);
  } else {
    return PyObject_TypeCheck (class, base->ob_type);
  }
}

PyObject *
ada_py_object_callmethod (PyObject *o, char *m)
{
  return PyObject_CallMethod (o, m, "");
}

PyObject *
ada_py_object_callmethod_obj (PyObject *o, char *m, PyObject *arg)
{
  return PyObject_CallMethod (o, m, "(O)", arg);
}

PyObject *
ada_py_object_callmethod_int (PyObject *o, char *m, int arg)
{
  PyObject_CallMethod (o, m, "(i)", arg);
}

int
ada_py_arg_parsetuple_ptr (PyObject *o, char *fmt, void *arg1)
{
  PyArg_ParseTuple (o, fmt, arg1);
}

int
ada_py_arg_parsetuple_ptr2 (PyObject *o, char *fmt, void *arg1, void *arg2)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2);
}

int
ada_py_arg_parsetuple_ptr3
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3);
}

int
ada_py_arg_parsetuple_ptr4
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3, void *arg4)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4);
}

int
ada_py_arg_parsetuple_ptr5
  (PyObject *o, char *fmt,
   void *arg1, void * arg2, void *arg3, void *arg4, void *arg5)
{
  PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4, arg5);
}

extern int gnat_argc;
extern char **gnat_argv;

int
ada_py_main ()
{
  return Py_Main (gnat_argc, gnat_argv);
}

PyObject*
ada_type_new (PyTypeObject* meta, char* name, PyObject* bases, PyObject* dict)
{
  PyTypeObject* m = meta;
  PyObject *args, *kwargs, *b=NULL;
  PyObject* result;
  PyObject* str;

  if (dict == NULL) {
    printf ("ada_type_new requires a non-null dict\n");
    return NULL;
  }

  if (meta == NULL) {
    m = &PyType_Type;
  }

  /* Construct the parameter list. Do not use keyword arguments, since the
     __init__ of the builtin types do not accept them, and tp_new will try to
     call __init__, resulting in an error
   */

  args   = PyTuple_New (3);
  kwargs = PyDict_New ();

  str = PyString_FromString (name);
  PyTuple_SET_ITEM (args, 0, str);    /* steal reference to str */

  if (bases == NULL) {
    b = PyTuple_New (0);
    PyTuple_SET_ITEM (args, 1, b);  /* steal ref to b */
  } else {
    PyTuple_SetItem (args, 1, bases);  /* increase refcount for bases */
  }

  PyTuple_SetItem (args, 2, dict); /* increase refcount for dict */

  result = PyType_Type.tp_new (m, args, kwargs);

  Py_XDECREF (args);
  Py_XDECREF (kwargs);

  return result;
}

int
ada_pydescr_newGetSet (PyTypeObject* type,
		       char*         name,
		       setter        set,
		       getter        get,
		       char*         doc,
		       void*         closure)
{
  struct PyGetSetDef *descr =
     (struct PyGetSetDef*)malloc (sizeof (struct PyGetSetDef));
  PyObject* prop;

  descr->name    = name;
  descr->get     = get;
  descr->set     = set;
  descr->doc     = doc;
  descr->closure = closure;

  prop = PyDescr_NewGetSet (type, descr);
  if (prop == NULL) {
    return 0;
  } else {
    PyObject_SetAttrString ((PyObject*)type, name, prop);
    return 1;
  }
}
