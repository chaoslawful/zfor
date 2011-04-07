#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
	struct hostent *h;

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <hostname>\n", argv[0]);
		exit(1);
	}

	h = gethostbyname(argv[1]);
	if (h) {
		struct in_addr *ap;
		int got = 0;
		char **p = h->h_addr_list;
		while (*p) {
			got = 1;
			ap = (struct in_addr *) *p;
			printf("%s resolved to %s\n", h->h_name, inet_ntoa(*ap));
			p++;
		}
		if (!got) {
			printf("failed to resolve %s\n", argv[1]);
		}
	} else {
		printf("gethostbyname failed!\n");
	}
}
