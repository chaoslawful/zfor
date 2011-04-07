#include <stdlib.h>
#include "zfor.h"
#include "util.h"
#include "sys_entry.h"
#include "imp_gethostbyname2.h"

struct hostent *zfor_gethostbyname2(const char *name, int af)
/* {{{ */
{
	// ZFOR only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname(name);
	}

	return zfor_sys_gethostbyname2(name, af);
}

/* }}} */


struct hostent *zfor_gethostbyname2i(const char *name, int af,
									 int failback)
/* {{{ */
{
	// ZFOR only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbynamei(name, failback);
	}

	return zfor_sys_gethostbyname2(name, af);
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
