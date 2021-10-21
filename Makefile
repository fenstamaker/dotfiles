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
# brew install pyenv python ansible
# brew install python
# ./scripts/brew-pyenv.sh link_brew_pyenv
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
# pyenv install --skip-existing 3.10.0
# pyenv global 3.10.0
# pyenv exec pip install --upgrade pip
# pyenv exec pip install --user pipx
# pyenv exec pipx install --force ansible-base
# pyenv exec pipx inject ansible-base ansible
# $(HOME)/.local/bin/ansible-galaxy collection install community.general


.PHONY: check
check:
	ansible-playbook -i hosts macos.yml --check -v

.PHONY: install
install:
ifdef MACOS
	ansible-playbook -i hosts macos.yml -v --ask-become-pass --skip-tags "work" --skip-tags "gui"
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

# .PHONY: bootstrap
# bootstrap:
# ifndef BREW_INSTALLED
# 	/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# endif
# 	brew analytics off
# 	brew update

# .PHONY: brew
# brew:
# 	brew bundle

# .PHONY: pyenv
# pyenv:
# # Python build environment. See https://github.com/pyenv/pyenv/wiki#suggested-build-environment
# 	brew install openssl readline sqlite3 xz zlib
# # Installing pyenv separate from brew bundle
# 	brew install pyenv

# .PHONY: python
# python:
# 	pip3 install virtualenvwrapper
# 	pip3 install -U clokta

# .PHONY: javascript
# javascript:
# 	npm install -g npm@latest
# 	npm install -g npm-upgrade

# .PHONY: zsh
# zsh:
# 	/usr/local/opt/fzf/install
# 	sudo dscl . -create /Users/$(USER) UserShell /usr/local/bin/zsh

# .PHONY: dotfiles
# dotfiles:
# 	mkdir -p $(HOME)/.zsh.d
# 	ln -snf $(CURDIR)/emacs/emacs.d $(HOME)/.emacs.d
# 	ln -snf $(CURDIR)/git/.gitconfig $(HOME)/.gitconfig
# 	ln -snf $(CURDIR)/git/.gitignore $(HOME)/.gitignore_global
# 	ln -snf $(CURDIR)/fzf/.fzf.zsh $(HOME)/.fzf.zsh
# 	ln -snf $(CURDIR)/p10k/.p10k.zsh $(HOME)/.p10k.zsh
# 	ln -snf $(CURDIR)/zsh/.zshrc $(HOME)/.zshrc
# 	ln -snf $(CURDIR)/zsh/.zprofile $(HOME)/.zprofile
# 	ln -snf $(CURDIR)/zsh/.zshenv $(HOME)/.zshenv
# 	ln -snf $(CURDIR)/eslint/.eslintrc $(HOME)/.eslintrc
# 	ln -snf $(CURDIR)/iterm2/com.googlecode.iterm2.plist $(HOME)/Library/Preferences/com.googlecode.iterm2.plist

# .PHONY: lsp
# lsp:
# 	npm i -g typescript-language-server
# 	npm i -g javascript-typescript-langserver
# 	npm i -g bash-language-server
# 	pip install ‘python-language-server[all]’

# .PHONY: install
# install: \
# 	dotfiles \
# 	pyenv \
# 	brew \
# 	zsh \
# 	javascript \
# 	python

# .PHONY: cleanup
# cleanup:
# 	brew cleanup
# 	brew bundle cleanup --force

# .PHONY: uninstall
# uninstall:
# 	pip3 uninstall clokta
# 	pip3 uninstall virtualenvwrapper
# # /usr/local/opt/fzf/uninstall
# 	brew cleanup
# 	rm -f $(HOME)/.emacs.d
# 	rm -f $(HOME)/.gitignore_global
# # rm -f $(HOME)/.zshrc
# # rm -rf $(HOME)/powerlevel9k

# .PHONY: env
# env:
# 	@echo $(PATH) | sed 's/:/\n/g'