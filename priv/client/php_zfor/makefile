all: modules/zfor.so

modules/zfor.so:
	phpize
	./configure --with-zfor
	make -f Makefile

clean:
	phpize --clean

.PHONY: all clean

