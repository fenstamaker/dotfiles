#export PATH="/usr/local/bin:$PATH"

.PHONY: bootstrap
bootstrap:
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	brew analytics off

.PHONY: brew
brew:
	brew upgrade
	brew install git
	brew install node
	brew install postgresql
	brew install python
	brew install python3
	brew install redis
	brew install tmux
	brew install tree
	brew install yarn

	# GUIs
	brew tap caskroom/cask
	brew cask install --force iterm2
	brew cask install --force spectacle
	brew cask install --force 1password
	brew cask install --force dash
	brew cask install --force emacs
	brew cask install --force psequel

	## Java
	brew tap AdoptOpenJDK/openjdk
	brew cask install java
	brew cask install adoptopenjdk8

	brew install kafka
	brew install clojure
	brew install leiningen
	brew install boot-clj

.PHONY: zsh
zsh:
	brew install zsh zsh-completions
	grep "$$(which zsh)" /etc/shells || echo "$$(which zsh)" | sudo tee -a /etc/shells
	echo $$SHELL | grep zsh || chsh -s "$$(which zsh)"
	if [ ! -d $(HOME)/powerlevel9k ]; then \
	  git clone https://github.com/bhilburn/powerlevel9k.git $(HOME)/powerlevel9k; \
	else \
	  git -C $(HOME)/powerlevel9k pull origin master; \
	fi

.PHONY: fonts
fonts:
	-brew tap caskroom/fonts
	-brew cask install font-source-code-pro
	-brew cask install font-source-code-pro-for-powerline
	-brew cask install font-sourcecodepro-nerd-font
	-brew cask install font-hack
	-brew cask install font-hack-nerd-font

.PHONY: dotfiles
dotfiles:
	ln -snf $(CURDIR)/dotfiles/emacs/.emacs.d $(HOME)/.emacs.d
	ln -snf $(CURDIR)/dotfiles/git/.gitignore $(HOME)/.gitignore_global
	ln -snf $(CURDIR)/dotfiles/env/.env $(HOME)/.env
	ln -snf $(CURDIR)/dotfiles/zsh/.zshenv $(HOME)/.zshenv
	ln -snf $(CURDIR)/dotfiles/zsh/.zshrc $(HOME)/.zshrc
	git config --global core.excludesfile $(HOME)/.gitignore_global

.PHONY: all
install: \
	brew \
	zsh \
	fonts

.PHONY: uninstall
uninstall:
	brew cleanup
	/usr/bin/ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
	rm -f $(HOME)/.emacs.d
	rm -f $(HOME)/.gitignore_global
	rm -f $(HOME)/.zshrc
	rm -rf $(HOME)/powerlevel9k
