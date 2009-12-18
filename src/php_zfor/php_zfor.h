#ifndef PHP_ZFOR_H__
#define PHP_ZFOR_H__

#define PHP_ZFOR_VERSION "1.0"
#define PHP_ZFOR_EXTNAME "zfor"

extern zend_module_entry zfor_module_entry;
#define php_ext_zfor_ptr &zfor_module_entry

#ifdef ZTS
#include "TSRM.h"
#endif

PHP_MINIT_FUNCTION(zfor);
PHP_MSHUTDOWN_FUNCTION(zfor);
PHP_MINFO_FUNCTION(zfor);

PHP_FUNCTION(zfor_set_udp_addr);
PHP_FUNCTION(zfor_set_udp_port);
PHP_FUNCTION(zfor_set_udp_timeout);
PHP_FUNCTION(zfor_gethostbyname);
PHP_FUNCTION(zfor_gethostbynamel);
PHP_FUNCTION(zfor_getvconf);

#endif
