#include <stdlib.h>
#include <string.h>
#include "zfor.h"
#include "util.h"
#include "sys_entry.h"
#include "imp_getaddrinfo.h"

int zfor_getaddrinfoi(const char *node, const char *service,
					  const struct addrinfo *hints, struct addrinfo **res,
					  int failback)
/* {{{ */
{
	int rc;
	struct hostent *h;
	char buf[sizeof(zfor_result_data) +
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

// vim600: noet ts=4 sw=4 fdm=marker
// vim<600: noet ts=4 sw=4
