SUBDIRS=src pkg

all:
	for i in $(SUBDIRS); do $(MAKE) -C $$i all; done

tgz:
	for i in $(SUBDIRS); do $(MAKE) ROOT=/opt/erlang_otp -C $$i all; done

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

.PHONY: all clean

