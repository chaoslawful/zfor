#include <netdb.h>
#include <errno.h>
#include <stdlib.h>
#include "zfor.h"
#include "util.h"
#include "imp_gethostbyname_r.h"

int zfor_gethostbyname_r(const char *name, struct hostent *ret, char *buf,
						 size_t buflen, struct hostent **result,
						 int *h_errnop)
/* {{{ */
{
	return zfor_gethostbyname_ri(name, ret, buf, buflen, result, h_errnop,
								 1);
}

/* }}} */

/**
 * gethostbyname_r() notes:
 * 	- 'buf' is used to store all the contents filled in (*ret) structure. If
 * 	it's not big enough, return ERANGE
 * 	- 'name', 'ret', 'buf', 'result', 'h_errnop' must be non-NULL, otherwise
 * 	the glibc2 implementation segfaults. Here we can return EFAULT, though.
 * 	- On success, (*ret) will be filled with resolving result, and (*result)
 * 	will point to 'ret', (*h_errnop) is set to 0, function returns 0.
 * 	- On error, (*ret) is not touched, and (*result) is set to 0, (*h_errnop)
 * 	is set to host error code HOST_NOT_FOUND (defined in 'netdb.h'), function
 * 	returns system error code.
 * 	*/
int zfor_gethostbyname_ri(const char *name, struct hostent *ret, char *buf,
						  size_t buflen, struct hostent **result,
						  int *h_errnop, int failback)
/* {{{ */
{
	struct hostent *h;

	if (!name || !ret || !buf || !result || !h_errnop) {
		return EFAULT;
	}

	if ((h = zfor_lookup(name, buf, buflen, h_errnop)) != NULL) {
		*h_errnop = 0;
		*ret = *h;
		*result = ret;
		return 0;
	}

	*result = NULL;

	// Buffer is not big enough to hold all resolving result
	if (errno == ENOMEM) {
		return ERANGE;
	}

	if (failback) {
		return zfor_sys_gethostbyname_r(name, ret, buf, buflen, result,
										h_errnop);
	}

	*h_errnop = HOST_NOT_FOUND;
	return errno;
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
