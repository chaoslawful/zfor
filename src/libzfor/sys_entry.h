#ifndef ZFOR_SYS_ENTRY_H__
#define ZFOR_SYS_ENTRY_H__

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#ifdef __cplusplus
extern "C" {
#endif

	int zfor_sys_getaddrinfo(const char *node, const char *service,
							 const struct addrinfo *hints,
							 struct addrinfo **res);
	struct hostent *zfor_sys_gethostbyname(const char *name);
	int zfor_sys_gethostbyname_r(const char *name, struct hostent *ret,
								 char *buf, size_t buflen,
								 struct hostent **result, int *h_errnop);
	struct hostent *zfor_sys_gethostbyname2(const char *name, int af);
	int zfor_sys_gethostbyname2_r(const char *name, int af,
								  struct hostent *ret, char *buf,
								  size_t buflen, struct hostent **result,
								  int *h_errnop);

#ifdef __cplusplus
}
#endif
#endif
/* vim600: noet ts=4 sw=4 fdm=marker */
/* vim<600: noet ts=4 sw=4 */
