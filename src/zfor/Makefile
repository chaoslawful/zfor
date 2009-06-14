SUBDIRS=
TEST_MODS=zfor_util zfor_config zfor_main

all: compile boot
	@for i in $(SUBDIRS); do $(MAKE) -C $$i all; done

am:
	@erl -s automake

compile:
	@erl -make
	@cp src/*.app ebin/
	@cp src/*.rel .

dialyze:
	dialyzer --succ_typings -c src/*.erl

boot: compile
	@erl -noshell -pa ./ebin -eval 'systools:make_script("zfor",[]),halt()'

test: compile
	@for mod in $(TEST_MODS); do \
		echo "Running unit-tests for $$mod: "; erl -noshell -pa ebin -s $$mod test -s init stop; \
	done

clean:
	@rm -f *~ ebin/* *.boot *.script *.tar.gz *.rel
	@for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done

.PHONY: all compile test

