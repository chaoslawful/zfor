#include <errno.h>
#include "zfor.h"
#include "util.h"
#include "imp_gethostbyname2_r.h"

int zfor_gethostbyname2_r(const char *name, int af, struct hostent *ret,
						  char *buf, size_t buflen,
						  struct hostent **result, int *h_errnop)
{
	// only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname_r(name, ret, buf, buflen, result,
									h_errnop);
	}

	if (h_errnop) {
		*h_errnop = HOST_NOT_FOUND;
	}

	return EPROTOTYPE;
}


int zfor_gethostbyname2_ri(const char *name, int af, struct hostent *ret,
						   char *buf, size_t buflen,
						   struct hostent **result, int *h_errnop,
						   int failback)
{
	// only support IPv4 address
	if (af == AF_INET) {
		return zfor_gethostbyname_ri(name, ret, buf, buflen, result,
									 h_errnop, failback);
	}

	if (h_errnop) {
		*h_errnop = HOST_NOT_FOUND;
	}

	return EPROTOTYPE;
}

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
