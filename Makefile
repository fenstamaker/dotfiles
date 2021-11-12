ifeq ($(OS), Windows_NT)
	WINDOWS := 1
else 
	UNAME := $(shell uname -s)
	UNAME_PROCESSOR := $(shell uname -m)
	ifeq ($(UNAME), Linux)
		LINUX := 1
	endif
	ifeq ($(UNAME), Darwin)
		MACOS := 1
		BREW_INSTALLED := $(shell command -v brew 2>/dev/null)
	endif
	ifeq ($(UNAME_PROCESSOR), arm64)
		ARM :=  1
	endif
endif

.PHONY: bootstrap
bootstrap:
ifeq ($(MACOS), 1)
ifndef BREW_INSTALLED
	/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
endif
ifeq ($(ARM), 1)
	/opt/homebrew/bin/brew analytics off 
	/opt/homebrew/bin/brew install ansible ansible-lint
else
	/usr/local/bin/brew analytics off 
	/usr/local/bin/brew install ansible ansible-lint
endif
endif
ifdef LINUX
	@echo "Uhh, oops."
	exit 1
endif
ifdef WINDOWS
	@echo "Uhh, oops."
	exit 1
endif
	ansible-galaxy collection install community.general



.PHONY: check
check:
	ansible-playbook -i hosts machine.yml --check -v

.PHONY: install
install:
ifdef MACOS
	eval "$(/opt/homebrew/bin/brew shellenv)"
	ansible-playbook -v \
		-i hosts \
		--ask-become-pass \
		--skip-tags "work" \
		--skip-tags "gui" \
		machine.yml 
endif
ifdef LINUX
	@echo "Uhh, oops."
	exit 1
endif
ifdef WINDOWS
	@echo "Uhh, oops."
	exit 1
endif


.PHONY: uninstall
uninstall:
	/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)"
