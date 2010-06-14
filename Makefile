

all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

docs:
	(cd src;$(MAKE) docs)

example:
	$(MAKE) -f Makefile.gettext

.PHONY: init
init:
	$(MAKE) -f Makefile.init

