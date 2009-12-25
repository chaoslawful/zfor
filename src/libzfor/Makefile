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

all: libzfor.so
	make -C utils

.c.o:
	$(CC) -c $(CCFLAGS) $^

libzfor.so: \
	hook.o \
	util.o \
	sys_entry.o \
	imp_getaddrinfo.o \
	imp_gethostbyname.o \
	imp_gethostbyname_r.o \
	imp_gethostbyname2.o \
	imp_gethostbyname2_r.o
	$(CC) $(LINK) $^ -o $@ $(LDFLAGS)

clean:
	make -C utils clean
	rm -f libzfor.so *.o

.PHONY: all clean

