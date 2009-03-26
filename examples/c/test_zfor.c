#include <zfor.h>
#include <stdio.h>
#include <netdb.h>

#define VIRTUALNAME "balance.test.com"

int main()
{
	struct hostent *res;
	res=zfor_gethostbyname(VIRTUALNAME);
	if(res!=NULL) {
		char **p;
		char buf[255];
		printf("h_name: %s\n",res->h_name);
		printf("h_addrtype: %s\n",(res->h_addrtype==AF_INET?"AF_INET":"AF_INET6"));
		printf("h_length: %d\n",res->h_length);
		printf("h_addr_list:\n");
		p=res->h_addr_list;
		while(*p!=NULL) {
			printf("\t%s\n",inet_ntop(res->h_addrtype,*p,buf,sizeof(buf)));
			p++;
		}
	} else {
		printf("Failed to resolve name " VIRTUALNAME "!\n");
	}
}

