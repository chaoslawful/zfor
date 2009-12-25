#include <stdlib.h>
#include "zfor.h"
#include "util.h"
#include "imp_gethostbyname2.h"

struct hostent *zfor_gethostbyname2(const char *name, int af)
/* {{{ */
{
	// only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname(name);
	}

	h_errno = HOST_NOT_FOUND;
	return NULL;
}

/* }}} */


struct hostent *zfor_gethostbyname2i(const char *name, int af,
									 int failback)
/* {{{ */
{
	// only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbynamei(name, failback);
	}

	h_errno = HOST_NOT_FOUND;
	return NULL;
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
