#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include "zfor.h"

int main(int argc,char *argv[])
{
	if(argc!=2) {
		fprintf(stderr,"Usage: %s <hostname>\n",argv[0]);
		exit(1);
	}

	struct hostent *h=zfor_gethostbyname(argv[1]);
	if(h) {
		bool got=false;
		char **p=h->h_addr_list;
		while(*p) {
			got=true;
			struct in_addr *ap=(struct in_addr*)*p;
			printf("%s resolved to %s\n",h->h_name,inet_ntoa(*ap));
			p++;
		}
		if(!got) {
			printf("failed to resolve %s\n",argv[1]);
		}
	} else {
		printf("zfor_gethostbyname failed!\n");
	}
}

