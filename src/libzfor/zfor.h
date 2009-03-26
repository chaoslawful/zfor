#ifndef ZFOR_H__
#define ZFOR_H__

#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Replacement for gethostbyname() in system libc
 * */
struct hostent *zfor_gethostbyname(const char *name);

/**
 * Internal DNS entry function
 * */
struct hostent *zfor_lookup(const char *name, void *resbuf, int resbuflen, int *errp);

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

