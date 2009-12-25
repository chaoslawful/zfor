#include <stdlib.h>
#include "zfor.h"
#include "util.h"
#include "sys_entry.h"
#include "imp_gethostbyname.h"

static char zfor_gethostbyname_static_buf[sizeof(zfor_result_data) +
										  ZFOR_MAX_HOSTADDRS *
										  sizeof(in_addr_t)];

struct hostent *zfor_gethostbynamei(const char *name, int failback)
/* {{{ */
{
	struct hostent *h;

	if ((h =
		 zfor_lookup(name, &zfor_gethostbyname_static_buf,
					 sizeof(zfor_gethostbyname_static_buf),
					 &h_errno)) != NULL) {
		return h;
	}

	if (failback) {
		return zfor_sys_gethostbyname(name);
	}

	return NULL;
}

/* }}} */

struct hostent *zfor_gethostbyname(const char *name)
/* {{{ */
{
	return zfor_gethostbynamei(name, 1);
}

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
