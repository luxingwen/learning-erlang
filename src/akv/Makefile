BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/akv
PRODRELPATH = _build/prod/rel/akv
DEV1RELPATH = _build/dev1/rel/akv
DEV2RELPATH = _build/dev2/rel/akv
DEV3RELPATH = _build/dev3/rel/akv
APPNAME = akv
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../akv_config
	[ -f $(RELPATH)/../akv_config/akv.conf ] || cp $(RELPATH)/etc/akv.conf  $(RELPATH)/../akv_config/akv.conf
	[ -f $(RELPATH)/../akv_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../akv_config/advanced.config

console:
	cd $(RELPATH) && ./bin/akv console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../akv_config
	[ -f $(PRODRELPATH)/../akv_config/akv.conf ] || cp $(PRODRELPATH)/etc/akv.conf  $(PRODRELPATH)/../akv_config/akv.conf
	[ -f $(PRODRELPATH)/../akv_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../akv_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/akv console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release
	mkdir -p $(DEV1RELPATH)/../akv_config
	[ -f $(DEV1RELPATH)/../akv_config/akv.conf ] || cp $(DEV1RELPATH)/etc/akv.conf  $(DEV1RELPATH)/../akv_config/akv.conf
	[ -f $(DEV1RELPATH)/../akv_config/advanced.config ] || cp $(DEV1RELPATH)/etc/advanced.config  $(DEV1RELPATH)/../akv_config/advanced.config

devrel2:
	$(REBAR) as dev2 release
	mkdir -p $(DEV2RELPATH)/../akv_config
	[ -f $(DEV2RELPATH)/../akv_config/akv.conf ] || cp $(DEV2RELPATH)/etc/akv.conf  $(DEV2RELPATH)/../akv_config/akv.conf
	[ -f $(DEV2RELPATH)/../akv_config/advanced.config ] || cp $(DEV2RELPATH)/etc/advanced.config  $(DEV2RELPATH)/../akv_config/advanced.config

devrel3:
	$(REBAR) as dev3 release
	mkdir -p $(DEV3RELPATH)/../akv_config
	[ -f $(DEV3RELPATH)/../akv_config/akv.conf ] || cp $(DEV3RELPATH)/etc/akv.conf  $(DEV3RELPATH)/../akv_config/akv.conf
	[ -f $(DEV3RELPATH)/../akv_config/advanced.config ] || cp $(DEV3RELPATH)/etc/advanced.config  $(DEV3RELPATH)/../akv_config/advanced.config

devrel: devrel1 devrel2 devrel3

dev1-attach:
	$(BASEDIR)/_build/dev1/rel/akv/bin/$(APPNAME) attach

dev2-attach:
	$(BASEDIR)/_build/dev2/rel/akv/bin/$(APPNAME) attach

dev3-attach:
	$(BASEDIR)/_build/dev3/rel/akv/bin/$(APPNAME) attach

dev1-console:
	$(BASEDIR)/_build/dev1/rel/akv/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/akv/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/akv/bin/$(APPNAME) console

devrel-clean:
	rm -rf _build/dev*/rel

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/akv/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/akv/bin/$(APPNAME)-admin cluster join akv1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/akv/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/akv/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/akv/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/akv/bin/$(APPNAME) ping; true; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/akv/bin/$(APPNAME) stop; true; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

