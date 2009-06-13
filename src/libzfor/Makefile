CXX=g++
CXXFLAGS=-g -O2 -fPIC
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
 LINK=-dynamiclib
 LDFLAGS=-Wl -ldl
else
 LINK=-shared
 LDFLAGS=-Wl,-rpath=$(PREFIX)/lib -ldl
endif

all: libzfor.so zfor_host override benchmark

.cc.o:
	$(CXX) -c $(CXXFLAGS) $^

libzfor.so: zfor.o
	$(CXX) $(LINK) $^ -o $@ $(LDFLAGS)

zfor_host: zfor_host.o libzfor.so
	$(CXX) zfor_host.o -o $@ $(LDFLAGS) -L. -lzfor

override: override.o
	$(CXX) $^ -o $@

benchmark: benchmark.o libzfor.so
	$(CXX) benchmark.o -o $@ $(LDFLAGS)

clean:
	rm -f libzfor.so zfor_host override benchmark *.o

.PHONY: all clean

