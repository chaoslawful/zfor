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
	char *hostname = NULL;
	struct hostent *hp;
	struct in_addr in;
	int failback = 1;

	if (!PyArg_ParseTuple(args, "s|b", &hostname, &failback)) {
		return NULL;
	}

	hp=zfor_gethostbyname2(hostname, failback);

	if(!hp || !hp->h_addr_list || !*(hp->h_addr_list)) {
		PyErr_SetString(zforError, "Name or service not known");
		return NULL;
	}

	in = *(struct in_addr*)*(hp->h_addr_list);

	return PyString_FromString(inet_ntoa(in));
}

static PyObject*
zgethostbynamel(PyObject *self, PyObject *args) {
	char *hostname=NULL;
	struct hostent *hp;
	struct in_addr in;
	PyObject *list;
	int i;

	int failback = 1;
	if (!PyArg_ParseTuple(args, "s|b", &hostname, &failback)) {
		return NULL;
	}

	hp=zfor_gethostbyname2(hostname, failback);

	if(!hp || !hp->h_addr_list || !*(hp->h_addr_list)) {
		PyErr_SetString(zforError, "Name or service not known");
		return NULL;
	}

	list = PyList_New(0);
	for(i = 0; hp->h_addr_list[i]!=NULL ;++i) {
		in=*(struct in_addr*)hp->h_addr_list[i];
		PyList_Append(list, PyString_FromString(inet_ntoa(in)));
	}
	return list;
}

static PyObject*
zgetvconf(PyObject *self, PyObject *args, PyObject* keywds) {

	char *vhost = NULL;
	char *prop = NULL;
	static char* kwlist[]={"vhost","prop",NULL};
	if (!PyArg_ParseTupleAndKeywords(args,keywds, "ss", kwlist,&vhost, &prop)) {
		return NULL;
	}

	char buf[4096] = { 0 };


	int len = zfor_getvconf(vhost,prop, buf,sizeof(buf));

	if(len < 0) {
		PyErr_SetString(zforError, "Bad prop or vhost name");
		return NULL;
	}

	return PyString_FromString(buf);
}

static PyObject*
zset_udp_port(PyObject *self, PyObject *args) {

	long port, old_port;
	if (!PyArg_ParseTuple(args, "l", &port)) {
		return NULL;
	}

	if (port <= 0 || port > 65535) {
		PyErr_SetString(zforError, "port value must in range(0,65536)");
		return NULL;
	}

	old_port = zfor_set_udp_port(port);

	return Py_BuildValue("l",old_port);
}

static PyObject*
zset_udp_timeout(PyObject *self, PyObject *args) {
	long timeout, old_timeout;
	if (!PyArg_ParseTuple(args, "l", &timeout)) {
		return NULL;
	}

	if (timeout < 0) {
		PyErr_SetString(zforError, "timeout value cant lower than 0");
		return NULL;
	}

	old_timeout = zfor_set_udp_timeout(timeout);

	return Py_BuildValue("l",old_timeout);
}

static PyMethodDef zforMethods[] = {
	{"gethostbyname",  zgethostbyname, METH_VARARGS, ""},
	{"gethostbynamel", zgethostbynamel, METH_VARARGS, ""},
	{"set_udp_port",  zset_udp_port, METH_VARARGS, ""},
	{"set_udp_timeout", zset_udp_timeout, METH_VARARGS, ""},
	{"getvconf", (PyCFunction)zgetvconf, METH_VARARGS | METH_KEYWORDS, ""},
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
