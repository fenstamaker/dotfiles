#export PATH="/usr/local/bin:$PATH"

.PHONY: bootstrap
bootstrap:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew analytics off

.PHONY: brew
brew:
	brew upgrade
	brew bundle

.PHONY: pip
pip:
	pip3 install virtualenvwrapper
	pip3 install -U clokta

.PHONY: zsh
zsh:
	/usr/local/opt/fzf/install
	grep "$$(which zsh)" /etc/shells || echo "$$(which zsh)" | sudo tee -a /etc/shells
	echo $$SHELL | grep zsh || chsh -s "$$(which zsh)"

.PHONY: dotfiles
dotfiles:
	mkdir -p $(HOME)/.zsh.d
	ln -snf $(CURDIR)/emacs/.emacs.d $(HOME)/.emacs.d
	ln -snf $(CURDIR)/git/.gitignore $(HOME)/.gitignore_global
	ln -snf $(CURDIR)/zsh/.zshrc $(HOME)/.zshrc
	ln -snf $(CURDIR)/zsh/.zprofile $(HOME)/.zprofile
	ln -snf $(CURDIR)/zsh/plugins $(HOME)/.zsh.d/plugins
	git config --global core.excludesfile $(HOME)/.gitignore_global

.PHONY: all
install: \
	brew \
	pip \
	zsh

.PHONY: uninstall
uninstall:
	pip3 uninstall clokta
	pip3 uninstall virtualenvwrapper
	/usr/local/opt/fzf/uninstall
	brew cleanup
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
	rm -f $(HOME)/.emacs.d
	rm -f $(HOME)/.gitignore_global
	rm -f $(HOME)/.zshrc
	rm -rf $(HOME)/powerlevel9k
	echo $$SHELL | grep zsh && chsh -s "$$(which bash)"
