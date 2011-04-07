#include "sys_entry.h"
#include "hook.h"
#include "util.h"

/*
 * Call system-wide gethostbyname() to resolve specified hostname
 * */
struct hostent *zfor_sys_gethostbyname(const char *name)
/* {{{ */
{
	static ptr_to_gethostbyname origin;

	if (!origin) {
		origin =
			(ptr_to_gethostbyname) zfor_getsym("gethostbyname", (void *)
											   zfor_hook_gethostbyname);
	}
	// 'origin' will never be NULL, because zfor_getsym() will abort() if that happens
	return origin(name);
}

/* }}} */

/*
 * Call system-wide getaddrinfo() to resolve specified hostname
 * */
int zfor_sys_getaddrinfo(const char *node, const char *service,
						 const struct addrinfo *hints,
						 struct addrinfo **res)
/* {{{ */
{
	static ptr_to_getaddrinfo origin;

	if (!origin) {
		origin = (ptr_to_getaddrinfo) zfor_getsym("getaddrinfo", (void *)
												  zfor_hook_getaddrinfo);
	}
	// 'origin' will never be NULL, because zfor_getsym() will abort() if that happens
	return origin(node, service, hints, res);
}

/* }}} */

/**
 * Call system-wide gethostbyname_r() to resolve specified hostname
 * */
int zfor_sys_gethostbyname_r(const char *name, struct hostent *ret,
							 char *buf, size_t buflen,
							 struct hostent **result, int *h_errnop)
/* {{{ */
{
	static ptr_to_gethostbyname_r origin;

	if (!origin) {
		origin =
			(ptr_to_gethostbyname_r) zfor_getsym("gethostbyname_r",
												 (void *)
												 zfor_hook_gethostbyname_r);
	}
	// 'origin' will never be NULL, because zfor_getsym() will abort() if that happens
	return origin(name, ret, buf, buflen, result, h_errnop);
}

/* }}} */

/**
 * Call system-wide gethostbyname2() to resolve specified hostname
 * */
struct hostent *zfor_sys_gethostbyname2(const char *name, int af)
/* {{{ */
{
	static ptr_to_gethostbyname2 origin;

	if (!origin) {
		origin =
			(ptr_to_gethostbyname2) zfor_getsym("gethostbyname2", (void *)
												zfor_hook_gethostbyname2);
	}
	// 'origin' will never be NULL, because zfor_getsym() will abort() if that happens
	return origin(name, af);
}

/* }}} */

/**
 * Call system-wide gethostbyname2_r() to resolve specified hostname
 * */
int zfor_sys_gethostbyname2_r(const char *name, int af,
							  struct hostent *ret, char *buf,
							  size_t buflen, struct hostent **result,
							  int *h_errnop)
/* {{{ */
{
	static ptr_to_gethostbyname2_r origin;

	if (!origin) {
		origin =
			(ptr_to_gethostbyname2_r) zfor_getsym("gethostbyname2_r",
												  (void *)
												  zfor_hook_gethostbyname2_r);
	}
	// 'origin' will never be NULL, because zfor_getsym() will abort() if that happens
	return origin(name, af, ret, buf, buflen, result, h_errnop);
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
