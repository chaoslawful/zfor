SUBDIRS=zfor libzfor

all:
	for i in $(SUBDIRS); do $(MAKE) -C $$i all; done

php:
	$(MAKE) -C php_zfor

erl:
	$(MAKE) -C erlang_zfor

java:
	$(MAKE) -C java_zfor

clean:
	for i in $(SUBDIRS) php_zfor erlang_zfor java_zfor; do $(MAKE) -C $$i clean; done

.PHONY: all clean

