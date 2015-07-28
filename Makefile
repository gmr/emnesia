PROJECT=awerl
REBAR=bin/rebar

all: compile

build-plt: all
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps kernel stdlib inets

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include

doc:
	@echo "Running rebar doc..."
	@$(REBAR) skip_deps=true doc

eunit:
	@echo "Running rebar eunit..."
	@$(REBAR) skip_deps=true eunit

run:
	@( erl -pa ebin deps/*/ebin -sname emnesia -config config/sys.config -sync log all -s emnesia )

run2:
	@( erl -pa ebin deps/*/ebin -sname emnesia2 -config config/sys.config -sync log all -s emnesia )

test: all eunit

xref:
	@$(REBAR) skip_deps=true xref

.PHONY: dialyze doc eunit xref
