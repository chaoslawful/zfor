#define _GNU_SOURCE
#include <stdlib.h>
#include "zfor.h"
#include "hook.h"
#include "util.h"

//////////////////////// Forward Declarations //////////////////////////

static int zfor_hook_failback = -1;

//////////////////////// Internal Functions //////////////////////////

int zfor_hook_getaddrinfo(const char *node, const char *service,
						  const struct addrinfo *hints,
						  struct addrinfo **res)
/* {{{ */
{
	return zfor_getaddrinfoi(node, service, hints, res,
							 zfor_hook_failback);
}

/* }}} */

struct hostent *zfor_hook_gethostbyname(const char *name)
/* {{{ */
{
	return zfor_gethostbynamei(name, zfor_hook_failback);
}

/* }}} */

int zfor_hook_gethostbyname_r(const char *name, struct hostent *ret,
							  char *buf, size_t buflen,
							  struct hostent **result, int *h_errnop)
/* {{{ */
{
	return zfor_gethostbyname_ri(name, ret, buf, buflen, result, h_errnop,
								 zfor_hook_failback);
}

/* }}} */

struct hostent *zfor_hook_gethostbyname2(const char *name, int af)
/* {{{ */
{
	return zfor_gethostbyname2i(name, af, zfor_hook_failback);
}

/* }}} */

int zfor_hook_gethostbyname2_r(const char *name, int af,
							   struct hostent *ret, char *buf,
							   size_t buflen, struct hostent **result,
							   int *h_errnop)
/* {{{ */
{
	return zfor_gethostbyname2_ri(name, af, ret, buf, buflen, result,
								  h_errnop, zfor_hook_failback);
}

/* }}} */

/**
 * ZFOR client hook initializer, set cache variable according to env ctl vars.
 * */
static void __attribute__ ((constructor)) zfor_hook_init(void)
/* {{{ */
{
	if (zfor_hook_failback == -1) {
		const char *hook_ctl = getenv(ZFOR_HOOK_CTL_ENV);

		if (hook_ctl) {
			if (!strncmp(hook_ctl, "nfb", 4)) {
				// Using hook callback without failing back
				zfor_hook_failback = 0;
			} else {
				// Default hook callback with failing back
				zfor_hook_failback = 1;
			}
		} else {
			// Default hook callback with failing back
			zfor_hook_failback = 1;
		}
	}
}

/* }}} */

/*
 * Define zfor_getaddrinfo() to be an alias of getaddrinfo(), in order to override
 * it by LD_PRELOAD
 * */
extern int getaddrinfo(const char *node, const char *service,
					   const struct addrinfo *hints, struct addrinfo **res)
	__attribute__ ((weak, alias("zfor_hook_getaddrinfo")));

/*
 * Define zfor_gethostbyname() to be an alias of gethostbyname(), in order to override
 * it by LD_PRELOAD
 * */
extern struct hostent *gethostbyname(const char *name)
	__attribute__ ((weak, alias("zfor_hook_gethostbyname")));

/*
 * Define zfor_gethostbyname_r() to be an alias of gethostbyname_r(), in order to override
 * it by LD_PRELOAD
 * */
extern int gethostbyname_r(const char *name, struct hostent *ret,
						   char *buf, size_t buflen,
						   struct hostent **result, int *h_errnop)
	__attribute__ ((weak, alias("zfor_hook_gethostbyname_r")));

/*
 * Define zfor_gethostbyname2() to be an alias of gethostbyname2(), in order to override
 * it by LD_PRELOAD
 * */
extern struct hostent *gethostbyname2(const char *name, int af)
	__attribute__ ((weak, alias("zfor_hook_gethostbyname2")));

/*
 * Define zfor_gethostbyname2_r() to be an alias of gethostbyname2_r(), in order to override
 * it by LD_PRELOAD
 * */
extern int gethostbyname2_r(const char *name, int af, struct hostent *ret,
							char *buf, size_t buflen,
							struct hostent **result, int *h_errnop)
	__attribute__ ((weak, alias("zfor_hook_gethostbyname2_r")));

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4

