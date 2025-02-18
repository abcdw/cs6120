GUIXTM=guix time-machine -C ./env/guile/cs6120/channels.scm
GUIX=$(GUIXTM) --
DEV_ENV=$(GUIX) shell -L env/guile guile-next guile-json guile-ares-rs \
	turnt brili bril-txt \
	-e '(@ (cs6120 packages) guix-from-core-channels)' \
	-e '(@ (cs6120 packages) core-channels-package)' \
--
GUILE=$(DEV_ENV) guile
GUILE_DEV=${GUILE} -L src/guile -L test/guile -L dev/guile -L env/guile

ares:
	$(GUILE_DEV) \
	-c \
	"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

.PHONY: env
env:
	$(DEV_ENV)

THIS_REPO=~/work/abcdw/cs6120
BRIL_REPO=~/work/sampsyo/bril

# TODO: [Andrew Tropin, 2025-02-18] Add config for turnt
check-tdce:
	$(DEV_ENV) turnt -c $(THIS_REPO)/src/turnt/tdce.toml \
	$(BRIL_REPO)/examples/test/tdce/*.bril
