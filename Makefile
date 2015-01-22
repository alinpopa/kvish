ERL ?= erl
APP := kvish

.PHONY: deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

start: all
	$(ERL) -pa $(PWD)/ebin $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s kvish

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
