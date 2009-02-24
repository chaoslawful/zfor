#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <sys/select.h>
#include <unistd.h>

int kbhit()
{
	static char buf[4096];
	struct timeval tv;
	fd_set rfds;
	tv.tv_sec=0;
	tv.tv_usec=0;

	FD_ZERO(&rfds);
	FD_SET(STDIN_FILENO,&rfds);

	if(select(STDIN_FILENO+1,&rfds,NULL,NULL,&tv)>0) {
		if(FD_ISSET(STDIN_FILENO,&rfds)) {
			read(STDIN_FILENO,buf,sizeof(buf));
			return 1;
		}
	}
	return 0;
}

int main(int argc,char *argv[])
{
	struct timeval begin,end;
	int cnt=0;
	if(argc!=2) {
		fprintf(stderr,"Usage: %s <hostname>\n",argv[0]);
		exit(1);
	}

	gettimeofday(&begin,NULL);
//	while(!kbhit()) {
	for(int i=0;i<10000;++i) {
		struct hostent *h=gethostbyname(argv[1]);
		if(!h) {
			perror("gethostbyname failed!");
		}
		cnt++;
	}
	gettimeofday(&end,NULL);

	printf("Total count: %ld\n",cnt);
	printf("Total time: %lg s\n",(end.tv_sec-begin.tv_sec)+1e-6*(end.tv_usec-begin.tv_usec));
}

