summary: ZFOR is a fail-over name resolver.
name: zfor
version: 1.0
release: 1
url: http://code.google.com/p/zfor/
vendor: YahooKoubei <http://cn.yahoo.com>
license: GPLv2
provides: zfor
source: http://zfor.googlecode.com/files/zfor-1.0.tar.gz
requires: erlang >= R11B

buildrequires: erlang >= R11B
buildroot: %{_tmppath}/%{name}-%{version}-%(%{__id} -u)

%description
ZFOR is a virtual hostname resolver that supports health checking and
load-balancing. It could be used to take over some VIP works.

%package devel
summary: Development files for ZFOR
group: Development/Libraries
requires: zfor = %{version}-%{release}

%description devel
This package contains the C header files needed for developing applications
with ZFOR.

%prep
%setup -q

%build
make PREFIX=%{_prefix} all

%install

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)

%files devel
%defattr(-,root,root)

%changelog

