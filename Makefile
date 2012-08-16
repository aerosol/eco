PROJECT = eco
DIALYZER = dialyzer
REBAR = rebar

all: app

app:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl tools inets crypto public_key ssl mnesia ssh

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

run: app
	erl -pa ebin -boot start_sasl -s eco -eco_plugins shell -eco_auto_init true

example: clean app
	rm -rf Mnesia.example*
	ls
	erl -pa ebin -boot start_sasl -sname example -s eco_example -eco_plugins shell -eco_auto_init true

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info


