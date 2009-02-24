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
 * 系统gethostbyname()调用替代品
 * */
struct hostent *zfor_gethostbyname(const char *name);

struct hostent *zfor_lookup(const char *name, void *resbuf, int resbuflen, int *errp);

/**
 * 设置ZFOR UDP服务端口
 * */
int zfor_set_udp_port(int port);

/**
 * 设置ZFOR UDP请求超时时间(ms)
 * */
int zfor_set_udp_timeout(int timeout);

#ifdef __cplusplus
}
#endif

#endif

