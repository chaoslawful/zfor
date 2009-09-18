#include <Python.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
 
#include "zfor.h"

static PyObject *zforError;
static PyObject *
zgethostbyname(PyObject *self, PyObject *args) {
  char *hostname=NULL;
  struct hostent *hp;
  struct in_addr in;

  if (!PyArg_ParseTuple(args, "s", &hostname)) {
    return NULL;
  }
  
  hp=zfor_gethostbyname(hostname);
  
  if(!hp || !hp->h_addr_list || !*(hp->h_addr_list)) {
      PyErr_SetString(zforError, "Name or service not known");
      return NULL;
  }
  
  in=*(struct in_addr*)*(hp->h_addr_list);
  
  return PyString_FromString(inet_ntoa(in));
}

static PyObject*
zgethostbynamel(PyObject *self, PyObject *args) {
    char *hostname=NULL;
    struct hostent *hp;
    struct in_addr in;
    PyObject *list;
    int i;
    
    if (!PyArg_ParseTuple(args, "s", &hostname)) {
      return NULL;
    }
    
    hp=zfor_gethostbyname(hostname);
    
    if(!hp || !hp->h_addr_list || !*(hp->h_addr_list)) {
        PyErr_SetString(zforError, "Name or service not known");
        return NULL;
    }
    
    list = PyList_New(0);
    for(i=0;hp->h_addr_list[i]!=NULL;++i) {
        in=*(struct in_addr*)hp->h_addr_list[i];
        PyList_Append(list, PyString_FromString(inet_ntoa(in)));
    }
    return list;
}


static PyMethodDef zforMethods[] = {

  {"gethostbyname",  zgethostbyname, METH_VARARGS, ""},
  {"gethostbynamel",  zgethostbynamel, METH_VARARGS, ""},

  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initzfor(void) {
    PyObject *m;

    m = Py_InitModule("zfor", zforMethods);
    if (m == NULL)
        return;
        
    zforError = PyErr_NewException("zfor.gaierror", NULL, NULL);
    Py_INCREF(zforError);
    PyModule_AddObject(m, "gaierror", zforError);
    
}
