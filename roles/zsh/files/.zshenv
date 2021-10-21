# OS variables
[ "$(uname -s)" = "Darwin" ] && export MACOS=1 && export UNIX=1
[ "$(uname -s)" = "Linux" ] && export LINUX=1 && export UNIX=1
uname -s | grep -q "_NT-" && export WINDOWS=1
grep -q "Microsoft" /proc/version 2>/dev/null && export UBUNTU_ON_WINDOWS=1

# In zsh, $path (lowercase) is an array tied to $PATH (uppercase)

# macOS default paths are defined in /etc/paths
# They are:
#   /usr/local/bin
#   /usr/bin
#   /bin
#   /usr/sbin
#   /sbin

# Temporarily sets $path here for non-interactive shells
# macOS overwrites the $path so have to set again in .zprofile

CUSTOM_PREPEND_PATH=(
  /usr/local/opt/coreutils/libexec/gnubin
  /usr/local/opt/gnu-sed/libexec/gnubin
  /usr/local/opt/python/libexec/bin
  ${HOME}/.local/bin
)

CUSTOM_APPEND_PATH=()

path=(
  $CUSTOM_PREPEND_PATH
  $path
  $CUSTOM_APPEND_PATH
)

EDITOR=vim

# Makes $path unique
typeset -U path