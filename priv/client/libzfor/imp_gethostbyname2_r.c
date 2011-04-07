#include <errno.h>
#include "zfor.h"
#include "util.h"
#include "sys_entry.h"
#include "imp_gethostbyname2_r.h"

int zfor_gethostbyname2_r(const char *name, int af, struct hostent *ret,
						  char *buf, size_t buflen,
						  struct hostent **result, int *h_errnop)
/* {{{ */
{
	// ZFOR only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname_r(name, ret, buf, buflen, result,
									h_errnop);
	}

	return zfor_sys_gethostbyname2_r(name, af, ret, buf, buflen, result,
									 h_errnop);
}

/* }}} */


int zfor_gethostbyname2_ri(const char *name, int af, struct hostent *ret,
						   char *buf, size_t buflen,
						   struct hostent **result, int *h_errnop,
						   int failback)
/* {{{ */
{
	// ZFOR only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname_ri(name, ret, buf, buflen, result,
									 h_errnop, failback);
	}

	return zfor_sys_gethostbyname2_r(name, af, ret, buf, buflen, result,
									 h_errnop);
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
