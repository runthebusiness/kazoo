PROJECT = ecallmgr
ROOT = ..
DIALYZER = dialyzer

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

LIB_EBINS = $(shell find $(ROOT)/lib -maxdepth 2 -name ebin -print)
PA = $(foreach LIB_EBIN,$(LIB_EBINS),-pa $(LIB_EBIN))

ERLC_OPTS = +debug_info +warn_export_all $(PA) \
	-I $(ROOT)/lib  # +bin_opt_info

DIRS = . \
	$(ROOT)/lib/whistle-1.0.0 \
	$(ROOT)/lib/whistle_amqp-1.0.0

.PHONY: all compile clean

all: compile

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ -pa $(ROOT)/lib $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ -pa $(ROOT)/lib $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell $(PA) \
	-pa test -eval "eunit:test([$(MODULES)], [verbose])" -s init stop

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) $(PA) \
                --plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
