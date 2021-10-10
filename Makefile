#export PATH="/usr/local/bin:$PATH"

.PHONY: bootstrap
bootstrap:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew analytics off

.PHONY: brew
brew:
	brew bundle

.PHONY: python
python:
	pip3 install virtualenvwrapper
	pip3 install -U clokta


.PHONY: javascript
javascript:
	npm install -g npm@latest
	npm install -g npm-upgrade

.PHONY: zsh
zsh:
	/usr/local/opt/fzf/install
	sudo dscl . -create /Users/$(USER) UserShell /usr/local/bin/zsh

.PHONY: dotfiles
dotfiles:
	mkdir -p $(HOME)/.zsh.d
	ln -snf $(CURDIR)/emacs/emacs.d $(HOME)/.emacs.d
	ln -snf $(CURDIR)/git/.gitconfig $(HOME)/.gitconfig
	ln -snf $(CURDIR)/git/.gitignore $(HOME)/.gitignore_global
	ln -snf $(CURDIR)/fzf/.fzf.zsh $(HOME)/.fzf.zsh
	ln -snf $(CURDIR)/p10k/.p10k.zsh $(HOME)/.p10k.zsh
	ln -snf $(CURDIR)/zsh/.zshrc $(HOME)/.zshrc
	ln -snf $(CURDIR)/zsh/.zprofile $(HOME)/.zprofile
	ln -snf $(CURDIR)/zsh/.zshenv $(HOME)/.zshenv
	ln -snf $(CURDIR)/eslint/.eslintrc $(HOME)/.eslintrc
	ln -snf $(CURDIR)/iterm2/com.googlecode.iterm2.plist $(HOME)/Library/Preferences/com.googlecode.iterm2.plist

.PHONY: lsp
lsp:
	npm i -g typescript-language-server
	npm i -g javascript-typescript-langserver
	npm i -g bash-language-server
	pip install ‘python-language-server[all]’

.PHONY: install
install: \
	brew \
	javascript \
	python \
	zsh

.PHONY: cleanup
cleanup:
	brew cleanup
	brew bundle cleanup --force

.PHONY: uninstall
uninstall:
	pip3 uninstall clokta
	pip3 uninstall virtualenvwrapper
	/usr/local/opt/fzf/uninstall
	brew cleanup
	rm -f $(HOME)/.emacs.d
	rm -f $(HOME)/.gitignore_global
	rm -f $(HOME)/.zshrc
	rm -rf $(HOME)/powerlevel9k
