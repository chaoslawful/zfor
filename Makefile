SUBDIRS=src
PREFIX=/usr/local

all:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i all; done

php:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i php; done

erl:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i erl; done

java:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i java; done

pkg:
	#	$(MAKE) PREFIX=$(PREFIX) -C pkg
	rm -rf build; \
	mkdir -p build/{BUILD,RPMS,SOURCES,SPECS,SRPMS}; \
	export builddir=$(shell pwd)/build; \
	export cwd=$(shell basename `pwd`); \
	cd ..; \
	tar czf /tmp/zfor.tar.gz $$cwd; \
	rpmbuild --define="_topdir $$builddir" -tb /tmp/zfor.tar.gz

php-pkg:
	$(MAKE) PREFIX=$(PREFIX) -C pkg php-pkg

clean:
	for i in $(SUBDIRS) pkg; do $(MAKE) PREFIX=$(PREFIX) -C $$i clean; done
	rm -rf build

.PHONY: all php erl java pkg php-pkg clean

