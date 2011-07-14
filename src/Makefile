
include ../vsn.mk
VSN=$(GETTEXT_VSN)

MODULES=gettext \
	gettext_compile \
	gettext_format \
	gettext_server \
	gettext_validate \
	gettext_validate_no_trans \
	gettext_validate_bad_case \
	gettext_validate_bad_punct \
	gettext_validate_bad_ftxt \
	gettext_validate_bad_stxt \
	gettext_validate_bad_html \
	gettext_validate_bad_ws \
	gettext_yaws_html \
	gettext_checker \
	gettext_sup \
	gettext_app \
	gettext_iso639 

EBIN_FILES=$(MODULES:%=../ebin/%.beam) ../ebin/gettext.app
ERL_FILES=$(MODULES:%=%.erl)

ERLC_FLAGS+=+debug_info

INCLUDES=gettext_internal.hrl

APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

#
# Targets
#

all: $(EBIN_FILES)

debug:
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) -Ddebug"

docs: ../doc/index.html

../doc/index.html: ../doc/overview.edoc $(ERL_FILES)  $(INCLUDES)
	erl -noshell -run edoc_run application "gettext" '".."' '[]'

clean:
	rm -f $(EBIN_FILES) ../doc/*.html ../doc/edoc-info \
	  ../doc/stylesheet.css ../doc/erlang.png

../ebin/%.beam: %.erl $(INCLUDES) Makefile
	erlc -o ../ebin $<

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

