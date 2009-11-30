#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "php_zfor.h"
#include "zfor.h"

static function_entry zfor_functions[]={
	PHP_FE(zfor_set_udp_port,NULL)
	PHP_FE(zfor_set_udp_timeout,NULL)
	PHP_FE(zfor_gethostbyname,NULL)
	PHP_FE(zfor_gethostbynamel,NULL)
	{NULL,NULL,NULL}
};

zend_module_entry zfor_module_entry={
#if ZEND_MODULE_API_NO>=20010901
	STANDARD_MODULE_HEADER,
#endif
	PHP_ZFOR_EXTNAME,
	zfor_functions,
	PHP_MINIT(zfor),
	PHP_MSHUTDOWN(zfor),
	NULL,
	NULL,
	PHP_MINFO(zfor),
#if ZEND_MODULE_API_NO>=20010901
	PHP_ZFOR_VERSION,
#endif
	STANDARD_MODULE_PROPERTIES
};

#ifdef COMPILE_DL_ZFOR
ZEND_GET_MODULE(zfor)
#endif

PHP_MINIT_FUNCTION(zfor)
{
	return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(zfor)
{
	return SUCCESS;
}

PHP_MINFO_FUNCTION(zfor)
{
	php_info_print_table_start();
	php_info_print_table_row(2,"Z Fail-Over Resolver","enabled");
	php_info_print_table_row(2,"Version",PHP_ZFOR_VERSION);
	php_info_print_table_end();
}

PHP_FUNCTION(zfor_set_udp_port)
{
	int argc=ZEND_NUM_ARGS();
	long port, old_port;
	if(zend_parse_parameters(argc TSRMLS_CC,"l",&port)==FAILURE) {
		return;
	}

	if(port <= 0 || port > 65535) {
		RETURN_FALSE;
	}

	old_port = zfor_set_udp_port(port);

	RETURN_LONG(old_port);
}

PHP_FUNCTION(zfor_set_udp_timeout)
{
	int argc=ZEND_NUM_ARGS();
	long timeout, old_timeout;
	if(zend_parse_parameters(argc TSRMLS_CC,"l",&timeout)==FAILURE) {
		return;
	}

	if(timeout < 0) {
		RETURN_FALSE;
	}

	old_timeout = zfor_set_udp_timeout(timeout);

	RETURN_LONG(old_timeout);
}

PHP_FUNCTION(zfor_gethostbyname)
{
	int argc=ZEND_NUM_ARGS();
	char *hostname=NULL;
	int hostname_len;
	struct hostent *hp;
	struct in_addr in;

	if(zend_parse_parameters(argc TSRMLS_CC,"s",&hostname,&hostname_len)==FAILURE) {
		return;
	}

	hp=zfor_gethostbyname(hostname);

	if(!hp || !hp->h_addr_list || !*(hp->h_addr_list)) {
		RETURN_FALSE;
	}

	in=*(struct in_addr*)*(hp->h_addr_list);
	RETURN_STRING(inet_ntoa(in),1);
}

PHP_FUNCTION(zfor_gethostbynamel)
{
	int argc=ZEND_NUM_ARGS();
	char *hostname=NULL;
	int hostname_len;
	struct hostent *hp;
	struct in_addr in;
	int i;

	if(zend_parse_parameters(argc TSRMLS_CC,"s",&hostname,&hostname_len)==FAILURE) {
		return;
	}

	hp=zfor_gethostbyname(hostname);
	if(!hp || !hp->h_addr_list) {
		RETURN_FALSE;
	}

	array_init(return_value);
	for(i=0;hp->h_addr_list[i]!=NULL;++i) {
		in=*(struct in_addr*)hp->h_addr_list[i];
		add_next_index_string(return_value,inet_ntoa(in),1);
	}
}

