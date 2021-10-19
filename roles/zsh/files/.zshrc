# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# **************************************************
#  Functions
# **************************************************

function exists() {
  COMMAND=$1
  command -v ${COMMAND} 1>/dev/null 2>&1
  return $?
}

function load-if-exists() {
  FILE=$1
  [ -f ${FILE} ] && source ${FILE}
}

## Fuzzy cd, searches through all directories recursively
function fd() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

## cd to a folder in ~/Projects
function pd() {
  local dir
  dir=$(exa -aD ~/Projects | fzf +m) && cd "${HOME}/Projects/${dir}"
}

function dotfiles {
  cd "${HOME}/dotfiles"
}

# **************************************************
#  Env Setup
# **************************************************

## pyenv
if exists pyenv; then
  eval "$(pyenv init -)"
fi

## jenv
if exists jenv; then
  path=(
    "${HOME}/.jenv/bin"
    $path
  )

  eval "$(jenv init -)"

  jenv enable-plugin export 1> /dev/null
  jenv enable-plugin maven 1> /dev/null
fi


# **************************************************
#  Loading Configs
# **************************************************

## fzf
load-if-exists ~/.fzf.zsh

## p10k
load-if-exists ~/.p10k.zsh

## z
load-if-exists /usr/local/etc/profile.d/z.sh 

## Homebrew completions
if exists brew; then
  fpath=(
    $fpath
    $(brew --prefix)/share/zsh/site-functions
  )
fi


# **************************************************
#  Zinit
# **************************************************

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

## Zinit Plugins
zinit ice depth=1; zinit light romkatv/powerlevel10k

zinit wait lucid for \
  wfxr/forgit \
  urbainvaes/fzf-marks \
  hlissner/zsh-autopair \
  changyuheng/zsh-interactive-cd \
  atinit"zicompinit; zicdreplay" \
    zdharma/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
    zsh-users/zsh-completions 


# **************************************************
#  zsh configuration
# **************************************************

setopt BANG_HIST
setopt EXTENDED_HISTORY

# Write to the history file immediately, not when the shell exits.
setopt APPEND_HISTORY
setopt SHARE_HISTORY

# Share history between all sessions.
setopt SHARE_HISTORY
# Expire duplicate entries first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST
# Don't record an entry that was just recorded again.
setopt HIST_IGNORE_DUPS
# Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_ALL_DUPS
# Do not display a line previously found.
setopt HIST_FIND_NO_DUPS
# Don't record an entry starting with a space.
setopt HIST_IGNORE_SPACE
# Don't write duplicate entries in the history file.
setopt HIST_SAVE_NO_DUPS
# Remove superfluous blanks before recording entry.
setopt HIST_REDUCE_BLANKS
# Don't execute immediately upon history expansion.
setopt HIST_VERIFY
# Beep when accessing nonexistent history.
setopt HIST_BEEP

# Excludes certain commands from history
# rm - Could include sensitve files
# echo - Could include passwords/keys
function zshaddhistory() {
    emulate -L zsh
    if [[ $1 =~ "\brm\b|^echo" ]] ; then
        return 1
    fi
}


# **************************************************
#  zsh variables
# **************************************************
KEYTIMEOUT=1
CASE_SENSITIVE="false"

FZF_COMPLETION_TRIGGER='**'
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=80

GREP_OPTIONS="--color=auto"
CLICOLOR=1



# **************************************************
#  Aliases
# **************************************************

alias reload="exec zsh -l"


alias java8="jenv shell 1.8"
alias java11="jenv shell 11"


alias l='exa -algF'
alias ll='exa -algF'
alias t='exa --tree --git-ignore'
alias tt='exa --tree --git-ignore'


alias sublime="open -a /Applications/Sublime\ Text.app"


# **************************************************
#  Final Setup
# **************************************************

autoload -U compinit && compinit

# Makes $path unique
typeset -U path