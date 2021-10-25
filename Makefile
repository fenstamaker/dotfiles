ifeq ($(OS), Windows_NT)
	WINDOWS := 1
else 
	UNAME := $(shell uname -s)
	ifeq ($(UNAME), Linux)
		LINUX := 1
	endif
	ifeq ($(UNAME), Darwin)
		MACOS := 1
		BREW_INSTALLED := $(shell command -v brew 2>/dev/null)
	endif
endif

.PHONY: bootstrap
bootstrap:
ifeq ($(MACOS), 1)
ifndef BREW_INSTALLED
	/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
endif
	brew analytics off 
	brew install ansible ansible-lint
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
	ansible-playbook -v \
		-i hosts \
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
