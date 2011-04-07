EMULATOR=beam
EBIN = $(ROOT)/ebin
INCLUDE = $(ROOT)/include

DIALYZER_OPT = -c --src \
			   --verbose
ERL_COMPILE_FLAGS += \
	-DDEBUG \
	+warn_unused_vars +warn_shadow_vars +warn_unused_import \
	+debug_info \
	+native \
	+"{hipe,[o3]}"
DOC_OPTS = {dir,\"../edoc\"}

SOURCES = \
	zfor_client

MODS = ${SOURCES:%=$(EBIN)/%.$(EMULATOR)} $(APP_TARGET) $(REL_TARGET)

$(EBIN)/%.$(EMULATOR):%.erl
	perl -i.bak -pe 's;%VSN%;$(VSN);g' $<
	-erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<
	mv $<.bak $<

all: $(MODS)

$(MODS): $(INCLUDE)/zfor_client.hrl

dialyze:
	dialyzer $(DIALYZER_OPT) ${SOURCES:%=%.erl}

edoc:
	erl -noshell -eval "edoc:application(${APPLICATION}, \".\", [${DOC_OPTS}]), init:stop()"

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(APP_TARGET) $(REL_TARGET) $(EBIN)/erl_crash.dump *~

