SUBDIRS=src
PREFIX=/usr/local

all:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i all; done

ext:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i ext; done

clean:
	for i in $(SUBDIRS); do $(MAKE) PREFIX=$(PREFIX) -C $$i clean; done

.PHONY: all clean

