TMP_DIR=/tmp/yk_zfor
TMP_ROOT=$(TMP_DIR)/opt/erlang_otp

all:
	if [ "$(ROOT)" = "/opt/erlang_otp" ]; then \
		sudo rm -rf $(TMP_DIR); \
		mkdir -p $(TMP_ROOT)/bin; \
		mkdir -p $(TMP_ROOT)/lib; \
		mkdir -p $(TMP_ROOT)/include; \
		mkdir -p $(TMP_ROOT)/share/zfor/ebin; \
		mkdir -p $(TMP_ROOT)/share/zfor/src; \
		mkdir -p $(TMP_ROOT)/share/zfor/include; \
		mkdir -p $(TMP_ROOT)/logs/zfor; \
		mkdir -p $(TMP_ROOT)/conf/zfor; \
		mkdir -p $(TMP_ROOT)/etc/zfor; \
		cp ../src/libzfor/libzfor.so $(TMP_ROOT)/lib/; \
		cp ../src/zfor/zfor.boot $(TMP_ROOT)/bin/; \
		cp ../bin/* $(TMP_ROOT)/bin/; \
		cp ../src/libzfor/zfor_host $(TMP_ROOT)/bin/; \
		cp ../src/zfor/ebin/* $(TMP_ROOT)/share/zfor/ebin/; \
		cp ../src/zfor/src/* $(TMP_ROOT)/share/zfor/src/; \
		cp ../src/zfor/include/* $(TMP_ROOT)/share/zfor/include/; \
		cp ../cfg/* $(TMP_ROOT)/conf/zfor/; \
		mv $(TMP_ROOT)/conf/zfor/erlang.cookie $(TMP_ROOT)/conf/zfor/.erlang.cookie; \
		cp ../src/libzfor/zfor.h $(TMP_ROOT)/include/; \
		perl -i -pe 's#\$$\(ROOT\)#$(ROOT)#g' $(TMP_ROOT)/conf/zfor/*; \
		perl -i -pe 's#\$$\(ROOT\)#$(ROOT)#g' $(TMP_ROOT)/bin/*; \
		sudo chown -R root:root $(TMP_ROOT); \
		sudo chown nobody:nogroup $(TMP_ROOT)/logs/zfor; \
		sudo chown nobody:nogroup $(TMP_ROOT)/conf/zfor/.erlang.cookie; \
		sudo chmod 0400 $(TMP_ROOT)/conf/zfor/.erlang.cookie; \
		sudo tar -czf ./zfor.tgz -C $(TMP_DIR) ./opt/erlang_otp; \
		sudo chmod a+rw ./zfor.tgz; \
	fi

clean:
	rm -rf *.tgz rhel*
	sudo rm -rf $(TMP_DIR)

.PHONY: all clean

