VER := $(shell grep Version ../../Changelog | head -1 | awk '{print $$3}')
MAJOR := $(shell echo $(VER) | perl -ne 'print $$1 if /^(\d+)(\.\d+(\.\d+)?)?$$/')
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

all: libzfor.so.$(VER)
	ln -sf libzfor.so.$(MAJOR) libzfor.so
	ln -sf $< libzfor.so.$(MAJOR)
	make -C utils

.c.o:
	$(CC) -c $(CCFLAGS) $^

libzfor.so.$(VER): \
	hook.o \
	util.o \
	sys_entry.o \
	imp_getaddrinfo.o \
	imp_gethostbyname.o \
	imp_gethostbyname_r.o \
	imp_gethostbyname2.o \
	imp_gethostbyname2_r.o
	$(CC) $(LINK) $^ -o $@ $(LDFLAGS) -Wl,-soname=libzfor.so.$(MAJOR)

clean:
	make -C utils clean
	rm -f libzfor.so.$(VER) libzfor.so.$(MAJOR) libzfor.so *.o

.PHONY: all clean

