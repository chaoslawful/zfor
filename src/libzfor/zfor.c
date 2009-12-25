#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "zfor.h"

#define ZFOR_CMD_DNS 0
#define ZFOR_CMD_GET_VCONF 1

#define ZFOR_HOOK_CTL_ENV "ZFOR_HOOK_CTL"	// ZFOR system lib hooking method, "fb" (default) / "nfb"
#define ZFOR_MAX_BUF_LEN 4096
#define ZFOR_UDP_ADDR 0x7f000001	// default to 127.0.0.1
#define ZFOR_UDP_PORT 1117
#define ZFOR_UDP_TIMEOUT 100
#define ZFOR_MAX_HOSTADDRS 16
#define ZFOR_MAX_ALIASES 0

typedef struct hostent *(*ptr_to_gethostbyname) (const char *name);
typedef int (*ptr_to_getaddrinfo) (const char *node, const char *service,
								   const struct addrinfo * hints,
								   struct addrinfo ** res);

struct zfor_result_data {
	struct hostent zfor_hostent;
	char zfor_name[MAXHOSTNAMELEN + 1];
	char *zfor_aliases[ZFOR_MAX_ALIASES + 1];
	in_addr_t *zfor_addr_list[ZFOR_MAX_HOSTADDRS + 1];
	int zfor_maxhosts;
};

static char zfor_static_buf[sizeof(struct zfor_result_data) +
							ZFOR_MAX_HOSTADDRS * sizeof(in_addr_t)];

static uint32_t zfor_udp_addr = ZFOR_UDP_ADDR;
static int zfor_udp_port = ZFOR_UDP_PORT;
static int zfor_udp_timeout = ZFOR_UDP_TIMEOUT;
static int zfor_hook_failback = 0;

//////////////////////// Forward Declarations //////////////////////////

static int zfor_make_req(char *buf, size_t len, char cmd, const void *data,
						 size_t data_len);
static void *zfor_getsym(const char *name, void *skip);
static struct hostent *zfor_sys_gethostbyname(const char *name);
static int zfor_sys_getaddrinfo(const char *node, const char *service,
								const struct addrinfo *hints,
								struct addrinfo **res);
static struct zfor_result_data *zfor_setup_result(void *resbuf,
												  int resbuflen);
static struct hostent *zfor_lookup(const char *name, void *resbuf,
								   int resbuflen, int *errp);
static int zfor_sync_call(struct sockaddr_in *host_and_port, int timeout,
						  const char *req, int reqlen, char *resp,
						  int maxresplen);
static struct hostent *zfor_hook_gethostbyname(const char *name);
static int zfor_hook_getaddrinfo(const char *node, const char *service,
								 const struct addrinfo *hints,
								 struct addrinfo **res);
static void zfor_lib_init(void);

/*
 * Define zfor_gethostbyname() to be an alias of gethostbyname(), in order to override
 * it by LD_PRELOAD
 * */
struct hostent *gethostbyname(const char *name)
	__attribute__ ((weak, alias("zfor_hook_gethostbyname")));

/*
 * Define zfor_getaddrinfo() to be an alias of getaddrinfo(), in order to override
 * it by LD_PRELOAD
 * */
int getaddrinfo(const char *node, const char *service,
				const struct addrinfo *hints, struct addrinfo **res)
	__attribute__ ((weak, alias("zfor_hook_getaddrinfo")));

//////////////////////// Exported API //////////////////////////

struct hostent *zfor_gethostbynamei(const char *name, int failback)
/* {{{ */
{
	struct hostent *h;

	if ((h =
		 zfor_lookup(name, &zfor_static_buf, sizeof(zfor_static_buf),
					 &h_errno)) != NULL) {
		return h;
	}

	if (failback) {
		return zfor_sys_gethostbyname(name);
	}

	return NULL;
}

/* }}} */

int zfor_getaddrinfoi(const char *node, const char *service,
					  const struct addrinfo *hints, struct addrinfo **res,
					  int failback)
/* {{{ */
{
	int rc;
	struct hostent *h;
	char buf[sizeof(struct zfor_result_data) +
			 ZFOR_MAX_HOSTADDRS * sizeof(in_addr_t)];

	if ((h = zfor_lookup(node, &buf, sizeof(buf), &h_errno)) != NULL) {
		// ZFOR resolving successful
		char abuf[INET_ADDRSTRLEN];
		struct addrinfo ai;
		struct addrinfo *pai, *tmp_res;
		struct addrinfo *prev;
		char **ap;

		// Convert the first resolved address into textual representation,
		// in order to make use of getaddrinfo()
		inet_ntop(AF_INET, h->h_addr_list[0], (char *) abuf, sizeof(abuf));

		// Copy resolving hints if it is given, or use default hints
		if (hints) {
			memcpy(&ai, hints, sizeof(ai));
		} else {
			memset(&ai, 0, sizeof(ai));

			// Only resolve IPv4 TCP/IP addresses
			ai.ai_family = AF_INET;
			ai.ai_socktype = SOCK_STREAM;
			ai.ai_protocol = IPPROTO_IP;
		}
		// Suppress potential lookups for numeric-form hostname
		ai.ai_flags |= AI_NUMERICHOST;

		// Call system original getaddrinfo() to fulfill the rest address informations
		if ((rc = zfor_sys_getaddrinfo(abuf, service, &ai, &tmp_res)) != 0) {
			return rc;
		}

		if (!h->h_addr_list[1]) {
			// There is only 1 resolved address, resolving completed
			*res = tmp_res;
			return 0;
		}
		// There are more than 1 resolved addresses, need to construct a 'struct addrinfo' list
		prev = NULL;
		for (ap = h->h_addr_list; *ap; ++ap) {
			pai =
				(struct addrinfo *) malloc(sizeof(*pai) +
										   tmp_res->ai_addrlen);

			if (pai) {
				// Copy address informations from getaddrinfo() result
				pai->ai_flags = tmp_res->ai_flags;
				pai->ai_family = tmp_res->ai_family;
				pai->ai_socktype = tmp_res->ai_socktype;
				pai->ai_protocol = tmp_res->ai_protocol;
				pai->ai_addrlen = tmp_res->ai_addrlen;
				pai->ai_addr = (struct sockaddr *) (pai + 1);
				memcpy(pai->ai_addr, tmp_res->ai_addr,
					   tmp_res->ai_addrlen);

				// Overwrite host address to the current resolved one
				((struct sockaddr_in *) pai->ai_addr)->sin_addr.s_addr =
					*((in_addr_t *) * ap);

				// Appending to list
				if (prev) {
					pai->ai_canonname = NULL;
					prev->ai_next = pai;
				} else {
					pai->ai_canonname = strdup(h->h_name);	// Duplicate canonical host name
					*res = pai;
				}
				pai->ai_next = NULL;
				prev = pai;
			} else {
				// Out of memory, free previous allocated resource by getaddrinfo() and return
				freeaddrinfo(tmp_res);
				return EAI_MEMORY;
			}
		}

		// Don't forget to free resource!
		freeaddrinfo(tmp_res);
		return 0;
	}
	// Failed to resolving host name through ZFOR service
	if (failback) {
		return zfor_sys_getaddrinfo(node, service, hints, res);
	}

	return EAI_NONAME;
}

/* }}} */

int zfor_getaddrinfo(const char *node, const char *service,
					 const struct addrinfo *hints, struct addrinfo **res)
/* {{{ */
{
	return zfor_getaddrinfoi(node, service, hints, res, 1);
}

/* }}} */

struct hostent *zfor_gethostbyname(const char *name)
/* {{{ */
{
	return zfor_gethostbynamei(name, 1);
}

/* }}} */

int zfor_getvconf(const char *vhost, const char *prop, char *buf,
				  int maxbuflen)
/* {{{ */
{
	char payload[ZFOR_MAX_BUF_LEN];
	char tmpbuf[ZFOR_MAX_BUF_LEN];
	int prop_len, vhost_len, payload_len;
	int len;
	struct sockaddr_in srvaddr;

	if (!vhost || !vhost[0]
		|| (vhost_len = strlen(vhost)) > sizeof(payload)) {
		return -1;
	}

	if (!prop || !prop[0] || (prop_len = strlen(prop)) > sizeof(payload)) {
		return -1;
	}

	payload_len = 1 + prop_len + vhost_len;
	if (payload_len > sizeof(payload)) {
		return -1;
	}

	if (!buf || maxbuflen <= 0) {
		return -1;
	}

	/* Construct ZFOR request payload */
	payload[0] = (char) prop_len;
	memcpy(payload + 1, prop, prop_len);
	memcpy(payload + prop_len + 1, vhost, vhost_len);

	/* Construct ZFOR request packet */
	len =
		zfor_make_req(tmpbuf, sizeof(tmpbuf), ZFOR_CMD_GET_VCONF, payload,
					  payload_len);
	if (len < 0) {
		return -1;
	}

	memset(&srvaddr, 0, sizeof(srvaddr));
	srvaddr.sin_addr.s_addr = htonl(zfor_udp_addr);
	srvaddr.sin_port = htons(zfor_udp_port);

	/* Synchronize call ZFOR service */
	len =
		zfor_sync_call(&srvaddr, zfor_udp_timeout * 1000, tmpbuf, len,
					   tmpbuf, sizeof(tmpbuf));
	if (len > 0 && maxbuflen >= len - 1) {	/* Make sure there are enough spaces for the result */
		if (tmpbuf[0] == 1) {
			/* Call completed successfully */
			memcpy(buf, tmpbuf + 1, len - 1);
			return len - 1;
		}
	}

	return -1;
}

/* }}} */

uint32_t zfor_set_udp_addr(uint32_t addr)
/* {{{ */
{
	uint32_t old_addr;

	old_addr = zfor_udp_addr;
	zfor_udp_addr = addr;

	return old_addr;
}

/* }}} */

int zfor_set_udp_port(int port)
/* {{{ */
{
	int old_port;

	if (port <= 0 || port > 65535) {
		return -1;
	}

	old_port = zfor_udp_port;
	zfor_udp_port = port;

	return old_port;
}

/* }}} */

int zfor_set_udp_timeout(int timeout)
/* {{{ */
{
	int old_timeout;

	if (timeout < 0) {
		return -1;
	}

	old_timeout = zfor_udp_timeout;
	zfor_udp_timeout = timeout;

	return old_timeout;
}

/* }}} */

//////////////////////// Declarations //////////////////////////

/*
 * Construct ZFOR UDP request packet
 * */
static int zfor_make_req(char *buf, size_t len, char cmd, const void *data,
						 size_t data_len)
/* {{{ */
{
	if (len < data_len + 1) {
		return -1;
	}
	buf[0] = cmd;
	memcpy(&buf[1], (const char *) data, data_len);
	return data_len + 1;
}

/* }}} */

/*
 * Retreive the original library entry of specified symbol
 * */
static void *zfor_getsym(const char *name, void *skip)
/* {{{ */
{
	void *f = NULL;

	f = dlsym(RTLD_NEXT, name);
	if (!f || f == skip) {
		f = dlsym(RTLD_DEFAULT, name);
		if (!f || f == skip) {
			void *l;

			// NOTE: This requires a real shared library named "libc.so" can be
			// found by ld.so. Most distributions make "/usr/lib/libc.so" a
			// linker script which could not be used here, BE CAREFUL!
			l = dlopen("libc.so", RTLD_LAZY);
			if (l) {
				f = dlsym(l, name);
				if (!f || f == skip) {
					abort();
				}
				dlclose(l);
			} else {
				abort();
			}
		}
	}

	return f;
}

/* }}} */

/*
 * Call system-wide gethostbyname() to resolve specified hostname
 * */
static struct hostent *zfor_sys_gethostbyname(const char *name)
/* {{{ */
{
	static ptr_to_gethostbyname origin;

	if (!origin) {
		origin =
			(ptr_to_gethostbyname) zfor_getsym("gethostbyname", (void *)
											   zfor_gethostbyname);
	}

	return origin(name);
}

/* }}} */

/*
 * Call system-wide getaddrinfo() to resolve specified hostname
 * */
static int zfor_sys_getaddrinfo(const char *node, const char *service,
								const struct addrinfo *hints,
								struct addrinfo **res)
/* {{{ */
{
	static ptr_to_getaddrinfo origin;

	if (!origin) {
		origin =
			(ptr_to_getaddrinfo) zfor_getsym("getaddrinfo",
											 (void *) zfor_getaddrinfo);
	}

	return origin(node, service, hints, res);
}

/* }}} */

/*
 * Verify the result buffer size, initialize the maximum allowed host address number
 * */
static struct zfor_result_data *zfor_setup_result(void *resbuf,
												  int resbuflen)
/* {{{ */
{
	struct zfor_result_data *p = (struct zfor_result_data *) resbuf;

	if (resbuflen < sizeof(struct zfor_result_data) + sizeof(in_addr_t)) {
		return NULL;
	}

	p->zfor_maxhosts =
		(resbuflen - sizeof(struct zfor_result_data)) / sizeof(in_addr_t);
	return p;
}

/* }}} */

static struct hostent *zfor_lookup(const char *name, void *resbuf,
								   int resbuflen, int *errp)
/* {{{ */
{
	struct in_addr addr;
	in_addr_t *ap;
	struct sockaddr_in srvaddr;
	char buf[ZFOR_MAX_BUF_LEN];
	int len;

	// Don't resolve empty or too long host names
	if (!name || !name[0] || strlen(name) > MAXHOSTNAMELEN) {
		*errp = NETDB_INTERNAL;
		errno = EINVAL;
		return NULL;
	}

	if (inet_pton(AF_INET, name, &addr) == 1) {
		// Specified host name is a valid IPv4 address, no need to resolve
		struct zfor_result_data *p = zfor_setup_result(resbuf, resbuflen);

		if (!p) {
			*errp = NETDB_INTERNAL;
			errno = ENOMEM;
			return NULL;
		}

		ap = (in_addr_t *) ((char *) p + sizeof(struct zfor_result_data));
		strcpy(p->zfor_name, name);
		p->zfor_aliases[0] = NULL;
		*ap = addr.s_addr;
		p->zfor_addr_list[0] = ap;
		p->zfor_addr_list[1] = NULL;

		p->zfor_hostent.h_name = p->zfor_name;
		p->zfor_hostent.h_aliases = p->zfor_aliases;
		p->zfor_hostent.h_addrtype = AF_INET;
		p->zfor_hostent.h_length = sizeof(in_addr_t);
		p->zfor_hostent.h_addr_list = (char **) p->zfor_addr_list;

		return &(p->zfor_hostent);
	}
	// Try resolve host name through ZFOR service

	memset(&srvaddr, 0, sizeof(srvaddr));
	srvaddr.sin_addr.s_addr = htonl(zfor_udp_addr);
	srvaddr.sin_port = htons(zfor_udp_port);

	len = zfor_make_req(buf, sizeof(buf),
						ZFOR_CMD_DNS, name, strlen(name));
	if (len < 0) {
		return NULL;
	}

	len =
		zfor_sync_call(&srvaddr, zfor_udp_timeout * 1000, buf, len, buf,
					   sizeof(buf));

	if (len > 0) {
		int i, total;

		// Get the resolved address number
		total = buf[0];

		// No resolving result or incorrect address number in ZFOR response,
		// turn to use system-wide gethostbyname()
		if (total * 4 + 1 != len || total > ZFOR_MAX_HOSTADDRS
			|| total == 0) {
			return NULL;
		}
		// Construct returning host entries according to addresses returned
		// by ZFOR.
		struct zfor_result_data *p = zfor_setup_result(resbuf, resbuflen);
		if (!p || p->zfor_maxhosts < total) {
			*errp = NETDB_INTERNAL;
			errno = ENOMEM;
			return NULL;
		}

		in_addr_t *ap =
			(in_addr_t *) ((char *) p + sizeof(struct zfor_result_data));
		strcpy(p->zfor_name, name);
		p->zfor_aliases[0] = NULL;
		for (i = 0; i < total; ++i) {
			// ZFOR returned address in network endian, no need to revert
			*ap = *(in_addr_t *) (buf + i * 4 + 1);
			p->zfor_addr_list[i] = ap;
			ap++;
		}
		p->zfor_addr_list[total] = NULL;

		p->zfor_hostent.h_name = p->zfor_name;
		p->zfor_hostent.h_aliases = p->zfor_aliases;
		p->zfor_hostent.h_addrtype = AF_INET;
		p->zfor_hostent.h_length = sizeof(in_addr_t);
		p->zfor_hostent.h_addr_list = (char **) p->zfor_addr_list;

		return &(p->zfor_hostent);
	}

	return NULL;
}

/* }}} */

static int zfor_sync_call(struct sockaddr_in *host_and_port, int timeout,
						  const char *req, int reqlen, char *resp,
						  int maxresplen)
/* {{{ */
{
	int rc;
	struct timeval tv;
	int sock;
	fd_set rfds;
	int nfds;
	int len;

	if (!req || reqlen <= 0 || !resp || maxresplen <= 0) {
		return -1;
	}

	sock = socket(AF_INET, SOCK_DGRAM, 0);

	if (sock < 0) {
		rc = -1;
	} else {
		/* Send request to ZFOR service */
		len =
			sendto(sock, req, reqlen, 0, (struct sockaddr *) host_and_port,
				   sizeof(struct sockaddr_in));

		if (len < 0) {
			rc = -1;
		} else {
			/* Request sent, wait for response */
			FD_ZERO(&rfds);
			FD_SET(sock, &rfds);

			tv.tv_sec = 0;
			tv.tv_usec = timeout;

			nfds = select(sock + 1, &rfds, NULL, NULL, &tv);

			if (nfds <= 0) {
				rc = -1;
			} else {
				len = recv(sock, resp, maxresplen, 0);

				if (len < 0) {
					rc = -1;
				} else {
					rc = len;
				}
			}
		}

		close(sock);
	}


	return rc;
}

/* }}} */

static struct hostent *zfor_hook_gethostbyname(const char *name)
/* {{{ */
{
	return zfor_gethostbynamei(name, zfor_hook_failback);
}

/* }}} */

static int zfor_hook_getaddrinfo(const char *node, const char *service,
								 const struct addrinfo *hints,
								 struct addrinfo **res)
/* {{{ */
{
	return zfor_getaddrinfoi(node, service, hints, res,
							 zfor_hook_failback);
}

/* }}} */

/**
 * ZFOR client library initializer, set cache variable according to env ctl vars.
 * */
static void __attribute__ ((constructor)) zfor_lib_init(void)
/* {{{ */
{
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

/* }}} */

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
