SUBDIRS=src
PREFIX=/usr/local

all:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i all; done

ext:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i ext; done

pkg:
	$(MAKE) PREFIX=$(PREFIX) -C pkg

ext-pkg:
	$(MAKE) PREFIX=$(PREFIX) -C pkg ext-pkg

clean:
	for i in $(SUBDIRS) pkg; do $(MAKE) PREFIX=$(PREFIX) -C $$i clean; done

.PHONY: all ext pkg ext-pkg clean

