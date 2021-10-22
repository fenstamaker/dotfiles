
# Reset macOS overwriting the $path variable
path=(
  $CUSTOM_PREPEND_PATH
  $path
  $CUSTOM_APPEND_PATH
)

if [ $MACOS -eq 1 ]; then 
  eval "$(pyenv init --path)"
else 
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
fi
