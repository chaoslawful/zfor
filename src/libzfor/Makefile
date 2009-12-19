CC := gcc
CCFLAGS := -g3 -O2 -fPIC
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	LINK := -dynamiclib
	LDFLAGS := -Wl -ldl
else
	LINK := -shared
	LDFLAGS := -Wl,-rpath=$(PREFIX)/lib64 -Wl,-rpath=$(PREFIX)/lib -ldl
endif

all: libzfor.so zfor_host zfor_prop override benchmark

.c.o:
	$(CC) -c $(CCFLAGS) $^

libzfor.so: zfor.o
	$(CC) $(LINK) $^ -o $@ $(LDFLAGS)

zfor_host: zfor_host.o libzfor.so
	$(CC) zfor_host.o -o $@ $(LDFLAGS) -L. -lzfor

zfor_prop: zfor_prop.o libzfor.so
	$(CC) zfor_prop.o -o $@ $(LDFLAGS) -L. -lzfor

override: override.o
	$(CC) $^ -o $@

benchmark: benchmark.o libzfor.so
	$(CC) benchmark.o -o $@ $(LDFLAGS)

clean:
	rm -f libzfor.so zfor_host zfor_prop override benchmark *.o

.PHONY: all clean

