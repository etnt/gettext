
include ../../../support/include.mk

include ../vsn.mk
VSN=$(GETTEXT_VSN)

MODULES=gettext \
	gettext_compile \
	gettext_format \
	gettext_server \
	gettext_checker \
	gettext_checking \
	gettext_sup \
	gettext_app \
	iso639 

EBIN_FILES=$(MODULES:%=../ebin/%.beam) ../ebin/gettext.app
ERL_FILES=$(MODULES:%=%.erl)

ERLC_FLAGS+=-W

INCLUDES=

#
# Targets
#

all: $(EBIN_FILES)

gettext_compile: ../ebin/gettext_compile.$(EMULATOR)

debug:
	$(MAKE) ERLC_FLAGS+="$(ERLC_FLAGS) +debug_info -Ddebug"

imsg:
	$(MAKE) ERLC_FLAGS+="-Dimsg"

docs: ../doc/index.html

../doc/index.html: $(ERL_FILES)  $(INCLUDES)
	erl -noshell -run edoc_run application "gettext" '".."' '[]'

clean:
	rm -f $(EBIN_FILES)

../ebin/*.$(EMULATOR): $(INCLUDES)
