all: dialyze

build:
	@./rebar compile

ct: clean build
	@./rebar ct

dialyze: build
	@./rebar dialyze

doc:
	@./rebar doc

clean:
	@./rebar clean
	@find . -name '*.beam' -exec rm -f '{}' ';'
	@rm -f doc/*.html doc/*.png doc/*.css doc/edoc-info
	@rm -rf logs/

.PHONY: all build ct dialyze doc clean

