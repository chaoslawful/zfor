CC := gcc
CCFLAGS := -g -O2 -fPIC
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	LINK := -dynamiclib
	LDFLAGS := -Wl -ldl
else
	LINK := -shared
	LDFLAGS := -Wl,-rpath=$(PREFIX)/lib64 -Wl,-rpath=$(PREFIX)/lib -ldl
endif

all: libzfor.so zfor_host override benchmark

.c.o:
	$(CC) -c $(CCFLAGS) $^

libzfor.so: zfor.o
	$(CC) $(LINK) $^ -o $@ $(LDFLAGS)

zfor_host: zfor_host.o libzfor.so
	$(CC) zfor_host.o -o $@ $(LDFLAGS) -L. -lzfor

override: override.o
	$(CC) $^ -o $@

benchmark: benchmark.o libzfor.so
	$(CC) benchmark.o -o $@ $(LDFLAGS)

clean:
	rm -f libzfor.so zfor_host override benchmark *.o

.PHONY: all clean

