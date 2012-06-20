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


#if PY_MAJOR_VERSION >= 3
PyMODINIT_FUNC
#else
PyObject *
#endif
ada_Py_InitModule4
  (char *name, PyMethodDef *methods,
   char *doc, PyObject *self)
{
#if PY_MAJOR_VERSION >= 3
   struct PyModuleDef def = {
     PyModuleDef_HEAD_INIT,
     name,                /* m_name */
     doc,                 /* m_doc */
     -1,                  /* m_size */
     methods,             /* m_methods */
     NULL,                /* m_reload */
     NULL,                /* m_traverse */
     NULL,                /* m_clear */
     NULL};               /* m_free */
   struct PyModuleDef* module = (struct PyModuleDef*)
         malloc(sizeof(struct PyModuleDef));
   PyObject* mod;
   PyObject* imported;

   memcpy(module, &def, sizeof(struct PyModuleDef));
   mod = PyModule_Create(module);

   return imported;
#else
   return Py_InitModule4 (name, methods, doc, self, PYTHON_API_VERSION);
#endif
}

#if PY_MAJOR_VERSION >= 3
// The definition of the module the user is creating via GNATCOLL.
// There is a single such module, so it is simpler to declare the
// variable as static rather than use calls to malloc().
static PyMethodDef user_methods[] = {
   {NULL, NULL}  /* Sentinel */
};
static struct PyModuleDef user_module = {
     PyModuleDef_HEAD_INIT,
     NULL,                /* m_name */
     NULL,                /* m_doc */
     -1,                  /* m_size */
     user_methods,        /* m_methods */
     NULL,                /* m_reload */
     NULL,                /* m_traverse */
     NULL,                /* m_clear */
     NULL                 /* m_free */
};
#endif

static char* user_module_name;

PyMODINIT_FUNC
init_user_module(void) {
   //struct PyModuleDef* module = (struct PyModuleDef*)malloc(sizeof(def));
   //memcpy(module, &def, sizeof(struct PyModuleDef));
#if PY_MAJOR_VERSION >= 3
   return PyModule_Create(&user_module);
#else
   Py_InitModule4 (user_module_name, NULL, "", NULL, PYTHON_API_VERSION);
#endif
};

//  To hide the output, we also need to rewrite displayhook.
//  Otherwise, calling a python function from Ada will print its
//  output to stdout (even though we have redirected sys.stdout ?)
//  So we make sure that nothing is ever printed. We cannot do this
//  systematically though, since in interactive mode (consoles...)
//  we still want the usual python behavior.

PyObject*
ada_py_initialize_and_module(char* program_name, char* name) {
   PyObject* module;
   PyObject* imported;

   user_module_name = strdup(name);

#if PY_MAJOR_VERSION >= 3
   user_module.m_name = user_module_name;
   Py_SetProgramName ((wchar_t*)program_name);
#else
   Py_SetProgramName (program_name);
#endif

   PyImport_AppendInittab(user_module_name, init_user_module);
   Py_InitializeEx(0);

   // Initialize the prompt if needed

   PyObject* prompt = PySys_GetObject ("ps1");
   if (prompt == NULL) {
      prompt = PyUnicode_FromString (">>> ");
      PySys_SetObject ("ps1", prompt);
      Py_DECREF (prompt);
   }

   prompt = PySys_GetObject ("ps2");
   if (prompt == NULL) {
      prompt = PyUnicode_FromString ("... ");
      PySys_SetObject ("ps2", prompt);
      Py_DECREF (prompt);
   }

   // Make the user's module visible to scripts. We cannot use
   // PyImport_ImportModule, which imports the module but doesn't add
   // it to the global dictionary and as such it is not visible to
   // user scripts.

   imported = PyImport_ImportModule(name);
   if (imported == NULL) {
      printf ("Could not import module %s", name);
      return NULL;
   }

   // Import 'sys', which is needed for instance in Set_Default_Console
   // to get access to the default value
   PyRun_SimpleString("import sys\n");

   char* command = (char*)malloc(9 + strlen(name));
   strcpy (command, "import ");
   strcat (command, name);
   strcat (command, "\n");
   PyRun_SimpleString(command);
   free (command);


   return imported;
};

void
ada_py_add_method(PyObject* cfunc, char* name, PyObject* class) {
#if PY_MAJOR_VERSION >= 3
   PyObject_SetAttrString (class, name, cfunc);
#else
   // Create an unbound method
   PyObject* cmeth = PyMethod_New (cfunc, NULL, class);
   PyObject_SetAttrString (class, name, cmeth);
   Py_DECREF (cmeth);
#endif
};

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
      snprintf (msg, 199, "%p (%s, rc=%ld)",
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
#if PY_MAJOR_VERSION >= 3
  return PyUnicode_Check (obj);
#else
  return PyString_Check (obj) || PyUnicode_Check (obj);
#endif
}

int
ada_pystring_check (PyObject* obj)
{
#if PY_MAJOR_VERSION >= 3
  return PyUnicode_Check (obj);
#else
  return PyString_Check (obj);
#endif
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
#if PY_MAJOR_VERSION >= 3
  return PyLong_Check (obj);
#else
  return PyInt_Check (obj);
#endif
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
  PyObject* kwtuple;
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
     kwtuple = PyTuple_New(2*nk);
     if (kwtuple == NULL)
       return NULL;
     k = &PyTuple_GET_ITEM(kwtuple, 0);
     pos = i = 0;
     while (PyDict_Next(kwds, &pos, &k[i], &k[i+1])) {
       Py_INCREF(k[i]);
       Py_INCREF(k[i+1]);
       i += 2;
     }
     nk = i/2;
  } else {
     k = NULL;
     nk = 0;
  }

#if PY_MAJOR_VERSION >= 3
  result = (PyObject*) PyEval_EvalCodeEx
    ((PyObject*) co,
     globals, locals,
     &PyTuple_GET_ITEM (args, 0) /* args */, PyTuple_Size (args) /* argc*/,
     k /* kwds */, nk /* kwdc */,
     d /* defs */, nd /* defcount */,
     NULL, /* kwdefs */
     closure /* closure */);
#else
  result = (PyObject*) PyEval_EvalCodeEx
    (co, globals, locals,
     &PyTuple_GET_ITEM (args, 0) /* args */, PyTuple_Size (args) /* argc*/,
     k /* kwds */, nk /* kwdc */,
     d /* defs */, nd /* defcount */,
     closure /* closure */);
#endif

  Py_XDECREF (kwtuple);
  return result;
}

int
ada_pycobject_check (PyObject* obj)
{
#if PY_MAJOR_VERSION >= 3
  return PyCapsule_CheckExact (obj);
#else
  return PyCObject_Check (obj);
#endif
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
ada_pymethod_check (PyObject* obj)
{
  return PyMethod_Check (obj);
}

PyTypeObject*
ada_gettypeobject (PyObject* obj)
{
  return (PyTypeObject*)(obj->ob_type);
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
  return PyObject_CallMethod (o, m, "(i)", arg);
}

int
ada_py_arg_parsetuple_ptr (PyObject *o, char *fmt, void *arg1)
{
  return PyArg_ParseTuple (o, fmt, arg1);
}

int
ada_py_arg_parsetuple_ptr2 (PyObject *o, char *fmt, void *arg1, void *arg2)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2);
}

int
ada_py_arg_parsetuple_ptr3
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3);
}

int
ada_py_arg_parsetuple_ptr4
  (PyObject *o, char *fmt, void *arg1, void * arg2, void *arg3, void *arg4)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4);
}

int
ada_py_arg_parsetuple_ptr5
  (PyObject *o, char *fmt,
   void *arg1, void * arg2, void *arg3, void *arg4, void *arg5)
{
  return PyArg_ParseTuple (o, fmt, arg1, arg2, arg3, arg4, arg5);
}

extern int gnat_argc;
extern char **gnat_argv;

int
ada_py_main ()
{
#if PY_MAJOR_VERSION >= 3
  return Py_Main (gnat_argc, (wchar_t**) gnat_argv);
#else
  return Py_Main (gnat_argc, (char**) gnat_argv);
#endif
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

#if PY_MAJOR_VERSION >= 3
  str = PyUnicode_FromString (name);
#else
  str = PyString_FromString (name);
#endif

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

#ifdef WITH_THREAD
const int python_with_thread = 1;
#else
const int python_with_thread = 0;
#endif

PyThreadState* ada_PyEval_SaveThread() {
#ifdef WITH_THREAD
   return PyEval_SaveThread();
#else
   return NULL;
#endif
}

void ada_PyEval_RestoreThread (PyThreadState* state) {
#ifdef WITH_THREAD
   PyEval_RestoreThread (state);
#endif
}

PyThreadState* ada_PyGILState_GetThisThreadState() {
#ifdef WITH_THREAD
   return PyGILState_GetThisThreadState();
#else
   return NULL;
#endif
}

int ada_PyGILState_Ensure() {
#ifdef WITH_THREAD
   return PyGILState_Ensure();
#else
   return 0;
#endif
}

void ada_PyEval_InitThreads() {
#ifdef WITH_THREAD
   PyEval_InitThreads();
#endif
}

int
ada_is_subclass (PyObject* class, PyObject* base)
{
  if (!class || !base) {
    return -1;
  } else {
    return PyObject_TypeCheck (class, base->ob_type);
  }
}

const char* ada_py_builtin() {
#if PY_MAJOR_VERSION >= 3
   return "builtins";
#else
   return "__builtin__";
#endif
}

const char* ada_py_builtins() {
#if PY_MAJOR_VERSION >= 3
   return "builtins";
#else
   return "__builtins__";
#endif
}

#if PY_MAJOR_VERSION >= 3

PyAPI_FUNC(PyObject *) PyInt_FromLong(long val) {
   return PyLong_FromLong(val);
};

PyAPI_FUNC(long) PyInt_AsLong(PyObject * val) {
   return PyLong_AsLong(val);
};

PyAPI_FUNC(PyObject *) PyString_FromStringAndSize(
      const char *val, Py_ssize_t s)
{
   return PyUnicode_FromStringAndSize(val, s);
};

PyAPI_FUNC(char *) PyString_AsString(PyObject * val) {
   PyObject* utf8 = PyUnicode_AsUTF8String(val);
   char* str = PyBytes_AS_STRING(utf8);
   Py_XDECREF(utf8);
   return str;
};

PyAPI_FUNC(void *) PyCObject_AsVoidPtr(PyObject * val) {
   void* data = PyCapsule_GetPointer(val, "GNATCOLL._C_API");
   return data;
};

PyAPI_FUNC(PyObject *) PyCObject_FromVoidPtr(
    void *cobj, void (*destruct)(void*))
{
   return PyCapsule_New(
         cobj /* pointer */,
         "GNATCOLL._C_API" /* name */,
         (PyCapsule_Destructor) destruct);
};

#endif
