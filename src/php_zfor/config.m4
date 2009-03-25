PHP_ARG_WITH(zfor, for ZFOR support,
[  --with-zfor             Include ZFOR support])

if test "$PHP_ZFOR" != "no"; then
	PHP_SUBST(ZFOR_SHARED_LIBADD)
	PHP_NEW_EXTENSION(zfor, php_zfor.c, $ext_shared)

	if test -z "$PREFIX"; then
		PREFIX=/usr/local
	fi

	PHP_ADD_INCLUDE(../libzfor)
	PHP_ADD_LIBRARY_WITH_PATH(zfor, ../libzfor, ZFOR_SHARED_LIBADD)

	CPPFLAGS="$CPPFLAGS -Werror -Wall -g -fno-strict-aliasing -Wl,-rpath=$PREFIX/lib"
fi

