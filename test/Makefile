MODULES=gettext_demo

EBIN_FILES=$(MODULES:%=../ebin/%.beam)
ERLC_FLAGS+=+debug_info

gettext: clean $(EBIN_FILES)

clean:
	@(rm -f $(EBIN_FILES))

../ebin/%.beam: %.erl $(INCLUDES) Makefile
	@(erlc -pa ../ebin -o ../ebin $(ERLC_FLAGS) $<)
