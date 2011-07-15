

all:
	(cd src;$(MAKE))

debug:
	(cd src;$(MAKE) debug)

clean:
	(cd src;$(MAKE) clean)

conf_clean:

docs:
	(cd src;$(MAKE) docs)

appfile:
	(cd src;$(MAKE) ../ebin/gettext.app)

example:
	$(MAKE) -f Makefile.gettext

.PHONY: init
init:
	$(MAKE) -f Makefile.init

