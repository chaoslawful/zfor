PREFIX=/usr/local
CFLAGS=-I$(PREFIX)/include
LDFLAGS=-L$(PREFIX)/lib -Wl,-rpath=$(PREFIX)/lib -lzfor

test: all
	@echo "*** Normal way:"
	./test_zfor
	@echo
	@echo "*** Preload way:"
	LD_PRELOAD=$(PREFIX)/lib/libzfor.so ./test_preload

all: test_zfor test_preload

test_preload: test_preload.c
	$(CC) -o $@ $^

%: %.c
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm test_zfor test_preload

.PHONY: all clean

