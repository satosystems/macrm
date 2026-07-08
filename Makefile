MAKEFILE := $(lastword $(MAKEFILE_LIST))
MAKEFILE_DIR := $(dir $(MAKEFILE))
SRCS := app/Main.hs $(wildcard src/*.hs) $(wildcard test/*.hs)

.PHONY: all
all: hie.yaml ## default target: build the project and generate hie.yaml
	cd $(MAKEFILE_DIR) && stack build --fast --pedantic

.PHONY: build
build: ## build the project
	cd $(MAKEFILE_DIR) && stack build --pedantic

.PHONY: clean
clean: ## clean the project
	cd $(MAKEFILE_DIR) && stack clean

.PHONY: rebuild
rebuild: clean build ## rebuild the project

.PHONY: install
install: rebuild installdirs ## install the project
	stack install

.PHONY: uninstall
uninstall: ## uninstall the project
	rm -f $${HOME}/.local/bin/macrm

.PHONY: distclean
distclean: ## clean the project and remove stack work directory and cabal files
	cd $(MAKEFILE_DIR) && rm -rf .stack-work macrm.cabal stack.yaml.lock

.PHONY: check
check: ## check the project
	stack test --coverage

.PHONY: test
test: check ## run the tests

.PHONY: lint
lint: ### run hlint on the project
	hlint src app test

.PHONY: installdirs
installdirs: ### create the directories for installation
	mkdir -p $(HOME)/.local/bin

.PHONY: man
man: share/man/man1/macrm.1 ## generate the man page
	man -M share/man macrm

.PHONY: pre-dist
pre-dist: /usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap/macrm.rb ## update the homebrew formula if the version has changed
	@if [ -f "$^" ]; then \
		$(eval CURRENT_VERSION := $(shell (cat $^ | grep url | awk -F'/' '{print $$8}'))) \
		$(eval NEW_VERSION := $(shell git tag | tail -n 1)) \
		if [ "$(CURRENT_VERSION)" != "$(NEW_VERSION)" ]; then \
			$(eval URL := $(shell echo "https://github.com/satosystems/macrm/releases/download/$(NEW_VERSION)/macrm.tar.gz")) \
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

hie.yaml: $(SRCS) ## generate the hie.yaml file for Haskell IDE Engine
	cd $(MAKEFILE_DIR) && type gen-hie 2> /dev/null && gen-hie > $@ || stack install implicit-hie && make -f $(MAKEFILE_DIR)$(MAKEFILE) $@

share/man/man1/macrm.1: share/man/man1/macrm.1.md package.yaml ## generate the man page from the markdown file
	$(eval VERSION := $(shell (cat package.yaml | grep ^version: | awk '{print $$2}')))
	cat $^ | sed "s/__VERSION__/$(VERSION)/" > temp.md
	pandoc temp.md -s -t man -o $@
	rm temp.md

macrm.tar.gz: share/man/man1/macrm.1 build ## create a tar.gz archive containing the binary and the man page
	$(eval BIN := $(shell (find .stack-work/dist -type f -name macrm)))
	mkdir -p .tmp
	cp $(BIN) $< .tmp
	cd .tmp && tar cvfz ../$@ macrm macrm.1
	rm -r .tmp

/usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap/macrm.rb: /usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap ## homebrew formula for macrm

/usr/local/Homebrew/Library/Taps/satosystems/homebrew-tap: ## create the homebrew tap for satosystems/homebrew-tap
	brew tap satosystems/tap

.PHONY: help
help: ## show this help
	@echo "Targets:"
	@awk 'BEGIN {FS=":.*## "}; \
		/^[a-zA-Z0-9_.-]+([ \t]+[a-zA-Z0-9_.-]+)*:.*## / { \
			n=split($$1, t, /[ \t]+/); \
			for (k=1; k<=n; k++) { \
				name=t[k]; d=$$2; \
				names[++i]=name; desc[i]=d; \
				if (length(name) > max) max = length(name); \
			} \
		} \
		END { \
			for (j=1; j<=i; j++) { \
				printf "  make %-" max "s - %s\n", names[j], desc[j]; \
			} \
		}' $(MAKEFILE)
