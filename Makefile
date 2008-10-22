
all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

docs:
	(cd src;$(MAKE) docs)

release: clean appfile
	sh ../../support/create_release.sh

appfile:
	(cd src;$(MAKE) ../ebin/gettext.app)

jungerl_example:
	$(MAKE) -f Makefile.gettext
