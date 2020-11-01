MAKEFILE := $(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR := $(dir $(MAKEFILE))
SRCS := app/Main.hs $(wildcard src/*.hs) $(wildcard test/*.hs)

.PHONY: all
all: hie.yaml
	cd $(MAKEFILE_DIR) && stack build --fast --pedantic

.PHONY: build
build:
	cd $(MAKEFILE_DIR) && stack build --pedantic

.PHONY: clean
clean:
	cd $(MAKEFILE_DIR) && stack clean

.PHONY: rebuild
rebuild: clean build

.PHONY: install
install: rebuild installdirs
	stack install

.PHONY: uninstall
uninstall:
	rm -f $${HOME}/.local/bin/macrm

.PHONY: distclean
distclean:
	cd $(MAKEFILE_DIR) && rm -rf .stack-work macrm.cabal stack.yaml.lock

.PHONY: check
check:
	stack test --coverage

.PHONY: test
test: check

.PHONY: lint
lint:
	hlint src app test

.PHONY: installdirs
installdirs:
	mkdir -p $(HOME)/.local/bin

.PHONY: pre-dist
pre-dist: /usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap/macrm.rb
	@if [ -f "$^" ]; then \
		$(eval CURRENT_VERSION := $(shell (cat $^ | grep url | awk -F'/' '{print $$8}'))) \
		$(eval NEW_VERSION := $(shell git tag | tail -n 1)) \
		if [ "$(CURRENT_VERSION)" != "$(NEW_VERSION)" ]; then \
			$(eval URL := $(shell echo "https://github.com/satosystems/macrm/releases/download/$(NEW_VERSION)/macrm")) \
			if [ "`curl -I $(URL) | head -n 1 | awk -F' ' '{print $$2}'`" != "404" ]; then \
				$(eval ESCAPED_URL := $(shell echo "$(URL)" | sed -E 's/\//\\\//g')) \
				curl -L -o "$(TMPDIR)macrm" "$(URL)"; \
				export __MACRM_HASH=`openssl sha256 "$(TMPDIR)macrm" | awk -F' ' '{print $$2}'`; \
				sed -E "/^  url /s/\"[^\"]+\"/\"$(ESCAPED_URL)\"/; /^  sha256 /s/\"[^\"]+\"/\"$${__MACRM_HASH}\"/" "$^" > "$(TMPDIR)macrm.rb" && mv "$(TMPDIR)macrm.rb" "$^"; \
				export -n __MACRM_HASH; \
				rm $(TMPDIR)macrm; \
			fi \
		fi \
	fi

hie.yaml: $(SRCS)
	cd $(MAKEFILE_DIR) && type gen-hie 2> /dev/null && gen-hie > $@ || stack install implicit-hie && make -f $(MAKEFILE_DIR)$(MAKEFILE) $@

/usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap/macrm.rb: /usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap

/usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap:
	brew tap satosystems/tap

help:
	@cat $(MAKEFILE) | grep -E '^[^ :]+ *:([^=]|$$)' | grep -v '^.PHONY *:' | awk -F':' '{print $$1}'
