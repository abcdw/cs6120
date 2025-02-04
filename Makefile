GUIXTM=guix time-machine -C ./env/channels-lock.scm
GUIX=$(GUIXTM) --
GUILE=$(GUIX) shell guile-next guile-json guile-ares-rs -- guile
GUILE_DEV=${GUILE} -L src/guile -L ./test/guile -L ./dev/guile

ares:
	$(GUILE) -L ./src/guile -c \
	"((@ (ares server) run-nrepl-server))"
