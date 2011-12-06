

all:
	(cd src;$(MAKE))

debug:
	(cd src;$(MAKE) debug)

clean:
	(cd src;$(MAKE) clean)

conf_clean:

docs:
	(cd src;$(MAKE) docs)

example:
	$(MAKE) -f Makefile.gettext

