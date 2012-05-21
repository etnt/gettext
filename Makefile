.PHONY: all debug test clean conf_clean docs example

all:
	(cd src;$(MAKE))

debug:
	(cd src;$(MAKE) debug)

test: all
	(cd test;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd test;$(MAKE) clean)
	rm -f priv/gettext_server_db.dets

conf_clean:

docs:
	(cd src;$(MAKE) docs)

example:
	$(MAKE) -f Makefile.gettext
