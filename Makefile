
all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

docs:
	(cd src;$(MAKE) docs)

appfile:
	(cd src;$(MAKE) ../ebin/gettext.app)

example:
	$(MAKE) -f Makefile.gettext
