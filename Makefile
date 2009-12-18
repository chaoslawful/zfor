SUBDIRS=src
PREFIX=/usr/local

all:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i all; done

ext:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i ext; done

erl:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i erl; done

pkg:
	#	$(MAKE) PREFIX=$(PREFIX) -C pkg
	rm -rf build; \
	mkdir -p build/{BUILD,RPMS,SOURCES,SPECS,SRPMS}; \
	export builddir=$(shell pwd)/build; \
	export cwd=$(shell basename `pwd`); \
	cd ..; \
	tar czf /tmp/zfor.tar.gz $$cwd; \
	rpmbuild --define="_topdir $$builddir" -tb /tmp/zfor.tar.gz


ext-pkg:
	$(MAKE) PREFIX=$(PREFIX) -C pkg ext-pkg

clean:
	for i in $(SUBDIRS) pkg; do $(MAKE) PREFIX=$(PREFIX) -C $$i clean; done
	rm -rf build

.PHONY: all ext pkg ext-pkg clean

