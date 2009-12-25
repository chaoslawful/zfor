CC := gcc
CCFLAGS := -g3 -O2 -fPIC -I..
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	LINK := -dynamiclib
	LDFLAGS := -Wl -ldl
else
	LINK := -shared
	LDFLAGS := -Wl,-rpath=$(PREFIX)/lib64 -Wl,-rpath=$(PREFIX)/lib -ldl
endif

all: zfor_host zfor_prop override benchmark

.c.o:
	$(CC) -c $(CCFLAGS) $^

zfor_host: zfor_host.o
	$(CC) zfor_host.o -o $@ $(LDFLAGS) -L.. -lzfor

zfor_prop: zfor_prop.o
	$(CC) zfor_prop.o -o $@ $(LDFLAGS) -L.. -lzfor

override: override.o
	$(CC) $^ -o $@

benchmark: benchmark.o
	$(CC) benchmark.o -o $@ $(LDFLAGS)

clean:
	rm -f zfor_host zfor_prop override benchmark *.o

.PHONY: all clean

