#ifndef ZFOR_UTIL_H__
#define ZFOR_UTIL_H__

#include <sys/param.h>			// for MAXHOSTNAMELEN

#define ZFOR_CMD_DNS 0
#define ZFOR_CMD_GET_VCONF 1

#define ZFOR_MAX_BUF_LEN 4096
#define ZFOR_UDP_ADDR 0x7f000001	// default to 127.0.0.1
#define ZFOR_UDP_PORT 1117
#define ZFOR_UDP_TIMEOUT 100
#define ZFOR_MAX_HOSTADDRS 16
#define ZFOR_MAX_ALIASES 0

typedef struct {
	struct hostent zfor_hostent;
	char zfor_name[MAXHOSTNAMELEN + 1];
	char *zfor_aliases[ZFOR_MAX_ALIASES + 1];
	in_addr_t *zfor_addr_list[ZFOR_MAX_HOSTADDRS + 1];
	int zfor_maxhosts;
} zfor_result_data;

#ifdef __cplusplus
extern "C" {
#endif

	void *zfor_getsym(const char *name, void *skip);

#ifdef __cplusplus
}
#endif
#endif
/* vim600: noet ts=4 sw=4 fdm=marker */
/* vim<600: noet ts=4 sw=4 */
