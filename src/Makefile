
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
	gettext_validate_bad_stxt \
	gettext_validate_bad_html \
	gettext_validate_bad_ws \
	gettext_yaws_html \
	gettext_checker \
	gettext_sup \
	gettext_app \
	gettext_iso639 

EBIN_FILES=$(MODULES:%=../ebin/%.beam)
ERL_FILES=$(MODULES:%=%.erl)

ERLC_FLAGS+=+debug_info

INCLUDES=gettext_internal.hrl

#
# Targets
#

all: $(EBIN_FILES) ../ebin/gettext.app

debug:
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) -Ddebug"

docs: ../doc/index.html

../doc/index.html: ../doc/overview.edoc $(ERL_FILES)  $(INCLUDES)
	erl -noshell -run edoc_run application "gettext" '".."' '[]'

clean:
	rm -f $(EBIN_FILES) ../ebin/gettext.app \
	 ../doc/*.html ../doc/edoc-info \
	 ../doc/stylesheet.css ../doc/erlang.png

../ebin/%.beam: %.erl $(INCLUDES) Makefile
	erlc -o ../ebin $(ERLC_FLAGS) $<

../ebin/gettext.app: gettext.app.src
	cp gettext.app.src ../ebin/gettext.app
