# OS variables
[ "$(uname -s)" = "Darwin" ] && export MACOS=1 && export UNIX=1
[ "$(uname -s)" = "Linux" ] && export LINUX=1 && export UNIX=1
uname -s | grep -q "_NT-" && export WINDOWS=1
grep -q "Microsoft" /proc/version 2>/dev/null && export UBUNTU_ON_WINDOWS=1

PATH=/usr/local/opt/python/libexec/bin:/usr/local/opt/coreutils/libexec/gnubin:$PATH
EDITOR=vim
HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000
