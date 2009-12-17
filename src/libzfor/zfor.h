#ifndef ZFOR_H__
#define ZFOR_H__

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netdb.h>

#ifdef __cplusplus
extern "C" {
#endif

	/**
	 * Replacement for gethostbyname() in system libc
	 * */
	struct hostent *zfor_gethostbyname(const char *name);

	/**
	 * Replacement for getaddrinfo() in system libc
	 * */
	int zfor_getaddrinfo(const char *node, const char *service, const struct addrinfo *hints, struct addrinfo **res);

	/**
	 * Set UDP port of zfor server
	 * */
	int zfor_set_udp_port(int port);

	/**
	 * Set UDP request timeout of zfor server
	 * */
	int zfor_set_udp_timeout(int timeout);

#ifdef __cplusplus
}
#endif
#endif

