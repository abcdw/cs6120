GUIXTM=guix time-machine -C ./env/guile/cs6120/channels.scm
GUIX=$(GUIXTM) --
GUILE=$(GUIX) shell -L env/guile guile-next guile-json guile-ares-rs \
	-e '(@ (cs6120 packages) guix-from-core-channels)' \
	-e '(@ (cs6120 packages) core-channels-package)' \
-- guile
GUILE_DEV=${GUILE} -L src/guile -L test/guile -L dev/guile -L env/guile

ares:
	$(GUILE_DEV) \
	-c "((@ (ares server) run-nrepl-server))"
