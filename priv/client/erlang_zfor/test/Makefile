INCLUDE = $(ROOT)/include
TEST_SERVER_INCLUDE = $(TEST_SERVER)/include
RUN_TEST_INCLUDE = $(RUN_TEST)/include
ERL_COMPILE_FLAGS += +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $(ERL_COMPILE_FLAGS) \
		-I$(INCLUDE) -I$(RUN_TEST_INCLUDE) -I$(TEST_SERVER_INCLUDE) \
		$<

.yrl.erl:
	erlc -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) $<

MODS = \
	   zfor_client_SUITE

all: compile

compile: ${MODS:%=%.beam}

clean:	
	rm -rf *.beam erl_crash.dump *~

