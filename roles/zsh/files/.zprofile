
# Reset macOS overwriting the $path variable
path=(
  $CUSTOM_PREPEND_PATH
  $path
  $CUSTOM_APPEND_PATH
)

HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

if command -v pyenv 1>/dev/null 2>&1; then
  if [ $MACOS -eq 1 ]; then 
    eval "$(pyenv init --path)"
  else 
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
  fi 
fi

# Makes $path unique
typeset -U path