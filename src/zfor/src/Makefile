EMULATOR=beam
EBIN = $(ROOT)/ebin
INCLUDE = $(ROOT)/include

APP_TARGET = $(EBIN)/zfor.app
REL_TARGET = $(ROOT)/zfor.rel
DIALYZER_OPT = -c --src \
			   --verbose \
			   --dump_callgraph /tmp/zfor_callgraph.dot

ERL_COMPILE_FLAGS += \
	-DDEBUG \
	+warn_unused_vars +warn_shadow_vars +warn_unused_import \
	+debug_info

SOURCES = \
	zfor_app\
	zfor_caretaker\
	zfor_config\
	zfor_hostlist\
	zfor_main\
	zfor_server\
	zfor_sup\
	zfor_util

MODS = ${SOURCES:%=$(EBIN)/%.$(EMULATOR)} $(APP_TARGET) $(REL_TARGET)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

all: $(MODS)

$(MODS): $(INCLUDE)/zfor_common.hrl

dialyze:
	dialyzer $(DIALYZER_OPT) ${SOURCES:%=%.erl}

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(APP_TARGET) $(REL_TARGET) $(EBIN)/erl_crash.dump *~

$(APP_TARGET): zfor.app.in Makefile
	sed -e 's;%VSN%;$(VSN);' $< | sed -e 's;%CONFPATH%;$(CONFPATH);' > $@

$(REL_TARGET): zfor.rel.in Makefile
	sed -e 's;%VSN%;$(VSN);' $< > $@

