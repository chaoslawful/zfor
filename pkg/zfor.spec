# ZFOR deployment structure:
#	/etc/rc.d/init.d/zfor	- init.d control script
#	/usr/local/bin/zfor*	- zfor admin tools
#	/etc/zfor/*		- zfor miscellaneous config files
#	/usr/local/etc/zfor/*	- zfor virtual host config files
#	/var/log/zfor/*		- zfor run-time log files
summary: ZFOR is a fail-over name resolver.
name: zfor
version: 1.0.4
release: 1
url: http://code.google.com/p/zfor/
vendor: Taobao <http://www.taobao.com>
license: GPLv2
group: Applications/System
provides: zfor
source: zfor.tar.gz
requires: erlang >= R11B

%define _tmppath /tmp
%define _prefix /usr/local
%define ext_root %(php-config --extension-dir)
%define ini_root %(php --ini|grep "Scan for additional .ini files"|perl -ane 'print $F[-1]')

buildrequires: erlang >= R11B, php-devel >= 5.2.9
buildroot: %{_tmppath}/%{name}-%{version}-%(%{__id} -u)

%package -n zfor-client
summary: ZFOR client library and header files
group: Development/Libraries

%package -n php-zfor-client
summary: ZFOR PHP extension
group: Applications/System
requires: zfor-client = %{version}-%{release}, php >= 5.2.9

%package -n erlang-zfor-client
summary: ZFOR Erlang client
group: Applications/System
requires: erlang >= R11B

%description
ZFOR is a virtual hostname resolver that supports health checking and
load-balancing. It could be used to take over some VIP works.

%description -n zfor-client
This package contains the library and  header files needed for developing
applications with ZFOR.

%description -n php-zfor-client
This package contains the PHP extension for ZFOR.

%description -n erlang-zfor-client
This package contains Erlang client module for ZFOR.

%prep
%setup -q -n zfor

%build
# build main files
make PREFIX=%{_prefix} all
# build php extension
make PREFIX=%{_prefix} ext
# build erlang client
make PREFIX=%{_prefix} erl

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{_prefix}/bin
mkdir -p %{buildroot}%{_prefix}/share/zfor/{ebin,src,include}
mkdir -p %{buildroot}%{_prefix}/etc/zfor
mkdir -p %{buildroot}/etc/zfor
mkdir -p %{buildroot}/var/log/zfor
mkdir -p %{buildroot}%{_prefix}/%{_lib}
mkdir -p %{buildroot}%{_prefix}/include
mkdir -p %{buildroot}%{ext_root}
mkdir -p %{buildroot}%{ini_root}
mkdir -p %{buildroot}/usr/%{_lib}/erlang/lib/zfor_client-%{version}/{ebin,src,include}

# install zfor admin tools & boot file
cp %{_builddir}/zfor/bin/* %{buildroot}%{_prefix}/bin/
mkdir -p %{buildroot}/etc/rc.d/init.d/
mv %{buildroot}%{_prefix}/bin/zfor %{buildroot}/etc/rc.d/init.d/
perl -i -pe 's#\$\(PREFIX\)#%{_prefix}#g' %{buildroot}%{_prefix}/bin/*
cp %{_builddir}/zfor/src/zfor/zfor.boot %{buildroot}%{_prefix}/bin/
# install zfor miscellaneous config files
cp %{_builddir}/zfor/cfg/* %{buildroot}/etc/zfor/
perl -i -pe 's#\$\(PREFIX\)#%{_prefix}#g' %{buildroot}/etc/zfor/*
# rename erlang cookie file
mv %{buildroot}/etc/zfor/erlang.cookie %{buildroot}/etc/zfor/.erlang.cookie
# install zfor main program files
cp %{_builddir}/zfor/src/zfor/ebin/* %{buildroot}%{_prefix}/share/zfor/ebin/
cp %{_builddir}/zfor/src/zfor/src/* %{buildroot}%{_prefix}/share/zfor/src/
cp %{_builddir}/zfor/src/zfor/include/* %{buildroot}%{_prefix}/share/zfor/include/
# install client library and header files
cp %{_builddir}/zfor/src/libzfor/zfor_host %{buildroot}%{_prefix}/bin/zfor-host
cp %{_builddir}/zfor/src/libzfor/zfor_prop %{buildroot}%{_prefix}/bin/zfor-prop
cp %{_builddir}/zfor/src/libzfor/libzfor.so %{buildroot}%{_libdir}
cp %{_builddir}/zfor/src/libzfor/zfor.h %{buildroot}%{_prefix}/include/
# install php extension files
cp %{_builddir}/zfor/src/php_zfor/modules/zfor.so %{buildroot}%{ext_root}/
cp %{_builddir}/zfor/src/php_zfor/zfor.ini %{buildroot}%{ini_root}/
# install erlang client files
cp %{_builddir}/zfor/src/erlang_zfor/ebin/*.beam %{buildroot}/usr/%{_lib}/erlang/lib/zfor_client-%{version}/ebin/
cp %{_builddir}/zfor/src/erlang_zfor/src/*.erl %{buildroot}/usr/%{_lib}/erlang/lib/zfor_client-%{version}/src/
cp %{_builddir}/zfor/src/erlang_zfor/include/*.hrl %{buildroot}/usr/%{_lib}/erlang/lib/zfor_client-%{version}/include/

%post
# post-install for zfor main package
if [ "$1" = "1" ]; then
	# real install, not upgrade
	/sbin/chkconfig --add zfor
	/sbin/chkconfig zfor on
fi

%preun
# pre-uninstall for zfor main package
if [ "$1" = "0" ]; then
	# real uninstall, not upgrade
	/etc/init.d/zfor stop
	/sbin/chkconfig --del zfor
fi

%files
# files for zfor main package
%defattr(-,root,root)
/etc/rc.d/init.d/zfor
%{_prefix}/bin/zfor.boot
%{_prefix}/bin/zfor-conf
%{_prefix}/bin/zfor-shell
%{_prefix}/bin/zfor-stat
%{_prefix}/bin/zfor-start
%{_prefix}/bin/zfor-stop
%{_prefix}/share/zfor
/etc/zfor
%attr(0400,nobody,nobody) /etc/zfor/.erlang.cookie
%dir %attr(0755,nobody,nobody) %{_prefix}/etc/zfor
%dir %attr(0755,nobody,nobody) /var/log/zfor

%files -n zfor-client
# files for zfor-client package
%defattr(-,root,root)
%{_prefix}/bin/zfor-host
%{_prefix}/include/zfor.h
%{_prefix}/%{_lib}/libzfor.so

%files -n php-zfor-client
# files for php-zfor-client package
%defattr(-,root,root)
%{ext_root}/zfor.so
%{ini_root}/zfor.ini

%files -n erlang-zfor-client
# files for erlang-zfor-client package
%defattr(-,root,root)
/usr/%{_lib}/erlang/lib/zfor_client-%{version}

%clean
rm -rf %{buildroot}

%changelog
* Fri Dec 18 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0.4-1
- added set zfor service address api
- added get virtual host config api

* Fri Dec 11 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0.3-1
- added erlang client

* Fri Nov 27 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0.2-1
- fixed bug: failed to resolve virtual hostname nor show zfor status when using
'grp_all' selection method

* Tue Nov 24 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0.1-1
- allow user to specify http method while performing health check
- allow user to specify http host header, in order to support virtual servers

* Mon Nov 09 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0-4
- added path to 'runuser' command

* Mon Nov 09 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0-3
- modified init.d script to replace 'sudo' with 'runuser'

* Thu Oct 29 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0-2
- modified zfor startup config to behave normally in badly configurated
enterprise environment

* Wed Oct 28 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0-1
- initial package

