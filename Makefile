REBAR_EXE = rebar
REBAR=if ! command -v $(REBAR_EXE) >/dev/null; then echo '$(REBAR_EXE) not found - please install first'; exit 1; fi ; $(REBAR_EXE)

.PHONY: all compile clean eunit xref dialyzer

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

eunit: all
	@$(REBAR) eunit

ct: all
	@$(REBAR) ct

xref: all
	@$(REBAR) xref
