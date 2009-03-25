SUBDIRS=zfor libzfor

all:
	for i in $(SUBDIRS); do $(MAKE) -C $$i all; done

ext:
	$(MAKE) -C php_zfor; done

clean:
	for i in $(SUBDIRS) php_zfor; do $(MAKE) -C $$i clean; done

.PHONY: all clean

