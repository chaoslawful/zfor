#define _GNU_SOURCE
#include <sys/param.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "zfor.h"
#include "util.h"

//////////////////////// Forward Declarations //////////////////////////

static int zfor_make_req(char *buf, size_t len, char cmd, const void *data,
						 size_t data_len);
static zfor_result_data *zfor_setup_result(void *resbuf, int resbuflen);
static int zfor_sync_call(struct sockaddr_in *host_and_port, int timeout,
						  const char *req, int reqlen, char *resp,
						  int maxresplen);

static uint32_t zfor_udp_addr = ZFOR_UDP_ADDR;
static int zfor_udp_port = ZFOR_UDP_PORT;
static int zfor_udp_timeout = ZFOR_UDP_TIMEOUT;

//////////////////////// Exported API //////////////////////////

/*
 * Retreive the original library entry of specified symbol
 * */
void *zfor_getsym(const char *name, void *skip)
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

struct hostent *zfor_lookup(const char *name, void *resbuf, int resbuflen,
							int *errp)
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
		zfor_result_data *p = zfor_setup_result(resbuf, resbuflen);

		if (!p) {
			*errp = NETDB_INTERNAL;
			errno = ENOMEM;
			return NULL;
		}

		ap = (in_addr_t *) ((char *) p + sizeof(zfor_result_data));
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

	len =
		zfor_make_req(buf, sizeof(buf), ZFOR_CMD_DNS, name, strlen(name));
	if (len < 0) {
		*errp = NETDB_INTERNAL;
		errno = ENOMEM;
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
			*errp = HOST_NOT_FOUND;
			errno = ETIMEDOUT;
			return NULL;
		}
		// Construct returning host entries according to addresses returned
		// by ZFOR.
		zfor_result_data *p = zfor_setup_result(resbuf, resbuflen);
		if (!p || p->zfor_maxhosts < total) {
			*errp = NETDB_INTERNAL;
			errno = ENOMEM;
			return NULL;
		}

		in_addr_t *ap =
			(in_addr_t *) ((char *) p + sizeof(zfor_result_data));
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

	*errp = HOST_NOT_FOUND;
	errno = ETIMEDOUT;
	return NULL;
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

//////////////////////// Internal Functions //////////////////////////

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
 * Verify the result buffer size, initialize the maximum allowed host address number
 * */
static zfor_result_data *zfor_setup_result(void *resbuf, int resbuflen)
/* {{{ */
{
	zfor_result_data *p = (zfor_result_data *) resbuf;

	if (resbuflen < sizeof(zfor_result_data) + sizeof(in_addr_t)) {
		return NULL;
	}

	p->zfor_maxhosts =
		(resbuflen - sizeof(zfor_result_data)) / sizeof(in_addr_t);
	return p;
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

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
