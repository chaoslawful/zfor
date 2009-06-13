#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/param.h>
#include <dlfcn.h>
#include <netdb.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "zfor.h"

#define ZFOR_UDP_PORT 1117
#define ZFOR_UDP_TIMEOUT 100
#define ZFOR_CMD_DNS 0
#define ZFOR_MAX_HOSTADDRS 16
#define ZFOR_MAX_ALIASES 0

typedef struct hostent* (*ptr_to_gethostbyname)(const char *name);

struct zfor_result_data {
	struct hostent zfor_hostent;
	char zfor_name[MAXHOSTNAMELEN+1];
	char *zfor_aliases[ZFOR_MAX_ALIASES+1];
	in_addr_t *zfor_addr_list[ZFOR_MAX_HOSTADDRS+1];
	int zfor_maxhosts;
};

static char zfor_static_buf[sizeof(struct zfor_result_data)+ZFOR_MAX_HOSTADDRS*sizeof(in_addr_t)];

static int zfor_udp_port=ZFOR_UDP_PORT;
static int zfor_udp_timeout=ZFOR_UDP_TIMEOUT;

// 构造ZFOR UDP请求数据包
static int zfor_make_req(char *buf, size_t len, char cmd, const void *data, size_t data_len)
{
	if(len<data_len+1) {
		return -1;
	}
	buf[0]=cmd;
	memcpy(&buf[1],(const char*)data,data_len);
	return data_len+1;
}

// 获取原始系统中的指定符号入口地址
static void* zfor_getsym(const char *name, void *skip)
{
	void *f=NULL;
	f=dlsym(RTLD_NEXT,name);
	if(!f || f==skip) {
		f=dlsym(RTLD_DEFAULT,name);
		if(!f || f==skip) {
			void *l;
			l=dlopen("libc.so",RTLD_LAZY);
			if(l) {
				f=dlsym(l,name);
				if(!f || f==skip) {
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

// 调用系统的gethostbyname()接口解析给定主机名
static struct hostent *zfor_sys_gethostbyname(const char *name)
{
	static ptr_to_gethostbyname byname;
	if(!byname) {
		byname=(ptr_to_gethostbyname)zfor_getsym("gethostbyname",(void*)zfor_gethostbyname);
	}
	return byname(name);
}

// 检验给定的缓冲区尺寸是否足够，并初始化最大允许保存的主机地址数量
static struct zfor_result_data *zfor_setup_result(void *resbuf, int resbuflen)
{
	struct zfor_result_data *p=(struct zfor_result_data*)resbuf;
	if(resbuflen<sizeof(struct zfor_result_data)+sizeof(in_addr_t)) {
		return NULL;
	}

	p->zfor_maxhosts=(resbuflen-sizeof(struct zfor_result_data))/sizeof(in_addr_t);
	return p;
}

// 定义zfor_gethostbyname为库函数调用gethostbyname的别名
extern "C" struct hostent *gethostbyname(const char *name) __attribute__((weak,alias("zfor_gethostbyname")));

struct hostent *zfor_gethostbyname(const char *name)
{
	return zfor_lookup(name,&zfor_static_buf,sizeof(zfor_static_buf),&h_errno);
}

struct hostent *zfor_lookup(const char *name, void *resbuf, int resbuflen, int *errp)
{
	// 不允许解析空主机名或超长主机名
	if(!name || !name[0] || strlen(name)>MAXHOSTNAMELEN) {
		*errp=NETDB_INTERNAL;
		errno=EINVAL;
		return NULL;
	}

	struct in_addr addr;
	if(inet_pton(AF_INET,name,&addr)==1) {
		// 给定主机名本身就是有效的IPv4地址，不需要进行域名解析
		struct zfor_result_data *p=zfor_setup_result(resbuf,resbuflen);
		if(!p) {
			*errp=NETDB_INTERNAL;
			errno=ENOMEM;
			return NULL;
		}
		in_addr_t *ap=(in_addr_t*)((char*)p+sizeof(struct zfor_result_data));
		strcpy(p->zfor_name,name);
		p->zfor_aliases[0]=NULL;
		*ap=addr.s_addr;
		p->zfor_addr_list[0]=ap;
		p->zfor_addr_list[1]=NULL;

		p->zfor_hostent.h_name=p->zfor_name;
		p->zfor_hostent.h_aliases=p->zfor_aliases;
		p->zfor_hostent.h_addrtype=AF_INET;
		p->zfor_hostent.h_length=sizeof(in_addr_t);
		p->zfor_hostent.h_addr_list=(char **)p->zfor_addr_list;

		return &(p->zfor_hostent);
	}

	struct sockaddr_in srvaddr;
	struct timeval tv;
	int sock;
	fd_set rfds;
	char buf[4096];
	int len;

	// 尝试连接ZFOR UDP服务端口解析给定域名
	sock=socket(AF_INET,SOCK_DGRAM,0);
	if(sock<0) {
		goto fallback;
	}

	memset(&srvaddr,0,sizeof(srvaddr));
	srvaddr.sin_addr.s_addr=htonl(0x7f000001); 	// 127.0.0.1
	srvaddr.sin_port=htons(zfor_udp_port);

	len=zfor_make_req(buf,sizeof(buf),ZFOR_CMD_DNS,name,strlen(name));
	if(len<0) {
		goto fallback;
	}

	if(sendto(sock,buf,len,0,(struct sockaddr*)&srvaddr,sizeof(srvaddr))<0) {
		goto fallback;
	}

	FD_ZERO(&rfds);
	FD_SET(sock,&rfds);

	tv.tv_sec=0;
	tv.tv_usec=zfor_udp_timeout*1000;

	if(select(sock+1,&rfds,NULL,NULL,&tv)>0) {
		int total;
		len=recv(sock,buf,sizeof(buf),0);

		// 关闭打开的UDP socket，释放资源
		close(sock);
		sock=-1;

		// 获得ZFOR解析地址数量
		total=buf[0];

		// 若没有解析结果或数量不对则改用系统gethostbyname()接口解析
		if(total*4+1!=len || total>ZFOR_MAX_HOSTADDRS || total==0) {
			goto fallback;
		}

		// 根据ZFOR返回的IP地址列表构造返回的hostent数据
		struct zfor_result_data *p=zfor_setup_result(resbuf,resbuflen);
		if(!p || p->zfor_maxhosts<total) {
			*errp=NETDB_INTERNAL;
			errno=ENOMEM;
			return NULL;
		}

		in_addr_t *ap=(in_addr_t*)((char*)p+sizeof(struct zfor_result_data));
		strcpy(p->zfor_name,name);
		p->zfor_aliases[0]=NULL;
		for(int i=0;i<total;++i) {
			// ZFOR返回的IP地址就是网络字节序，不需要颠倒
			*ap=*(in_addr_t*)(buf+i*4+1);
			p->zfor_addr_list[i]=ap;
			ap++;
		}
		p->zfor_addr_list[total]=NULL;

		p->zfor_hostent.h_name=p->zfor_name;
		p->zfor_hostent.h_aliases=p->zfor_aliases;
		p->zfor_hostent.h_addrtype=AF_INET;
		p->zfor_hostent.h_length=sizeof(in_addr_t);
		p->zfor_hostent.h_addr_list=(char**)p->zfor_addr_list;

		return &(p->zfor_hostent);
	}

fallback:
	// ZFOR查询失败，使用系统gethostbyname调用尝试解析给定主机名
	if(sock>=0) {
		// 若UDP socket已打开则关闭之
		close(sock);
	}

	return zfor_sys_gethostbyname(name);
}

int zfor_set_udp_port(int port)
{
	int old_port=zfor_udp_port;
	zfor_udp_port=port;
	return old_port;
}

int zfor_set_udp_timeout(int timeout)
{
	int old_timeout=zfor_udp_timeout;
	zfor_udp_timeout=timeout;
	return old_timeout;
}

