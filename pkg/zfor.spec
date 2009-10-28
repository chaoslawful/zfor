# ZFOR deployment structure:
#	/etc/rc.d/init.d/zfor	- init.d control script
#	/usr/local/bin/zfor*	- zfor admin tools
#	/etc/zfor/*		- zfor miscellaneous config files
#	/usr/local/etc/zfor/*	- zfor virtual host config files
#	/var/log/zfor/*		- zfor run-time log files
summary: ZFOR is a fail-over name resolver.
name: zfor
version: 1.0
release: 1
url: http://code.google.com/p/zfor/
vendor: Taobao <http://www.taobao.com>
license: GPLv2
group: Applications/System
provides: zfor
source: http://zfor.googlecode.com/files/zfor-1.0.tar.gz
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
requires: zfor = %{version}-%{release}

%package -n php-zfor-client
summary: ZFOR PHP extension
group: Applications/System
requires: zfor = %{version}-%{release}, php >= 5.2.9

%description
ZFOR is a virtual hostname resolver that supports health checking and
load-balancing. It could be used to take over some VIP works.

%description -n zfor-client
This package contains the library and  header files needed for developing
applications with ZFOR.

%description -n php-zfor-client
This package contains the PHP extension for ZFOR.

%prep
%setup -q -n zfor

%build
# build main files
make PREFIX=%{_prefix} all
# build php extension
make PREFIX=%{_prefix} ext

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{_prefix}/bin
mkdir -p %{buildroot}%{_prefix}/share/zfor/{ebin,src,include}
mkdir -p %{buildroot}%{_prefix}/etc/zfor
mkdir -p %{buildroot}/etc/zfor
mkdir -p %{buildroot}/var/log/zfor
mkdir -p %{buildroot}%{_prefix}/lib64
mkdir -p %{buildroot}%{_prefix}/include
mkdir -p %{buildroot}%{ext_root}
mkdir -p %{buildroot}%{ini_root}

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
cp %{_builddir}/zfor/src/libzfor/libzfor.so %{buildroot}%{_prefix}/lib64/
cp %{_builddir}/zfor/src/libzfor/zfor.h %{buildroot}%{_prefix}/include/
# install php extension files
cp %{_builddir}/zfor/src/php_zfor/modules/zfor.so %{buildroot}%{ext_root}/
cp %{_builddir}/zfor/src/php_zfor/zfor.ini %{buildroot}%{ini_root}/

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
%{_prefix}/lib64/libzfor.so

%files -n php-zfor-client
# files for php-zfor-client package
%defattr(-,root,root)
%{ext_root}/zfor.so
%{ini_root}/zfor.ini

%clean
rm -rf %{buildroot}

%changelog
* Wed Oct 28 2009 qingwu <qingwu@taobao.com>
+ zfor-1.0-1
- initial package

