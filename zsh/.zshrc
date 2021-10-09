# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


# uautoload -Uz compinit
# compinit

if command -v pyenv 1>/dev/null 2>&1; then
    # (The below instructions are intended for common
    # shell setups. See the README for more guidance
    # if they don't apply and/or don't work for you.)

    # Add pyenv executable to PATH and
    # enable shims by adding the following
    # to ~/.profile and ~/.zprofile:

    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"

    # Load pyenv into the shell by adding
    # the following to ~/.zshrc:

    eval "$(pyenv init -)"

    # Make sure to restart your entire logon session
    # for changes to profile files to take effect.
fi

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
unset JAVA_TOOL_OPTIONS

jenv enable-plugin export 1> /dev/null
jenv enable-plugin maven 1> /dev/null

alias java-versions="/usr/libexec/java_home -V"
alias java8="jenv shell 1.8"
alias java11="jenv shell 11"
alias java-latest="jenv shell 15"

# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
# source /usr/local/bin/virtualenvwrapper.sh

# export NVM_DIR="$HOME/.nvm"
# [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
# [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

## Autocompletion
if type brew &>/dev/null; then
  HOMEBREW_PREFIX=$(brew --prefix)
  FPATH=${HOMEBREW_PREFIX}/share/zsh/site-functions:$FPATH
  FPATH=${HOMEBREW_PREFIX}/share/zsh-completions:$FPATH
fi

## Useful tools
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f /usr/local/etc/profile.d/z.sh ] && source /usr/local/etc/profile.d/z.sh


source /usr/local/share/antigen/antigen.zsh

antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zdharma/fast-syntax-highlighting
antigen bundle changyuheng/zsh-interactive-cd
antigen bundle wfxr/forgit
antigen bundle hlissner/zsh-autopair

antigen theme romkatv/powerlevel10k
antigen apply


setopt NO_HUP
# Treat the '!' character specially during expansion.
setopt BANG_HIST
# Write the history file in the ":start:elapsed;command" format.
setopt EXTENDED_HISTORY
# Write to the history file immediately, not when the shell exits.
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
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

## Plugins
POWERLEVEL9K_MODE='nerdfont-complete'

## Hooks

function zshaddhistory() {
    emulate -L zsh
    if [[ $1 =~ "\brm\b|^echo" ]] ; then
        return 1
    fi
}

## Config

# Shortens how long to wait when using ESC/Alt
KEYTIMEOUT=1
CASE_SENSITIVE="false"

FZF_COMPLETION_TRIGGER='**'
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=80

GREP_OPTIONS="--color=auto"
CLICOLOR=1

## Functions
git-add() {
    git add $1
    git status
}

git-clone() {
    git clone $1
    git submodule update --init
}

git-branch-from-main() {
    if [ -z $1 ]; then
        echo "Missing branch name"
        exit 1
    fi

    MAIN_EXISTS=$(git ls-remote --heads origin main)
    if [ -z $MAIN_EXISTS ]; then
        BRANCH="main"
    else
        BRANCH="master"
    fi

    git checkout $BRANCH
    git pull origin $BRANCH
    git checkout -b $1
}

git-main() {
  MAIN_EXISTS=$(git ls-remote --heads origin main)

  if [ -z $MAIN_EXISTS ]; then
    echo "main branch does not exist"
  else 
    echo "main branch exists, switching..."
    git pull
    git remote set-head origin -a
    git checkout main
  fi
}

gpp() {
    BRANCH=$1
    if [ -z $BRANCH ]; then
        BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
    fi
    git push origin $BRANCH
}

gpl() {
    BRANCH=$1
    if [ -z $BRANCH ]; then
        BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
    fi
    git pull origin $BRANCH
}

url-encode() {
    local string="${1}"
    local strlen=${#string}
    local encoded=""
    local pos c o

    for (( pos=0 ; pos<strlen ; pos++ )); do
        c=${string:$pos:1}
        case "$c" in
            [-_.~a-zA-Z0-9] ) o="${c}" ;;
            * )               printf -v o '%%%02x' "'$c"
        esac
        encoded+="${o}"
    done
    echo "${encoded}"    # You can either set a return variable (FASTER)
    REPLY="${encoded}"   #+or echo the result (EASIER)... or both... :p
}

url-decode() {
    # urldecode <string>

    local url_encoded="${1//+/ }"
    printf '%b' "${url_encoded//%/\\x}"
}
# Print line from file
# line 10,13 will print lines 10-13
line() {
    sed -n "${1}p" $2
}

copy-nile() {
    STAGE=$1
    cat nile.yml | \
        yq -c ".environments[] | select( .name | contains(\"$STAGE\") )" | \
        jq -c ".override.services | to_entries | first | .value.environment" | \
        jq -r 'to_entries | map(.key + "=" + (.value|tostring)) | join("\n")'
}

yaml() {
    read -r INPUT
    yamllint <(echo $INPUT)
}

# Fuzzy cd
fd() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

pd() {
  local dir
  dir=$(find ~/Projects -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# Go up parent directories
fdr() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local DIR=$(get_parent_dirs $(realpath "${1:-$PWD}") | fzf-tmux --tac)
  cd "$DIR"
}

# Find file and cd into containing dir
fdf() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# Fuzzy history search
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# Find text in project
vg() {
  local file
  local line

  read -r file line <<<"$(ag --nobreak --noheading $@ | fzf -0 -1 | awk -F: '{print $1, $2}')"

  if [[ -n $file ]]
  then
     emacs $file +$line
  fi
}

# CD to common directories
# unalias z
# z() {
#   if [[ -z "$*" ]]; then
#     cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
#   else
#     _last_z_args="$@"
#     _z "$@"
#   fi
# }

# zz() {
#   cd "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q "$_last_z_args")"
# }

lower() {
    echo "$1" | tr '[:upper:]' '[:lower:]'
}

upper() {
    echo "$1" | tr '[:lower:]' '[:upper:]'
}

mkvenv() {
    if [ -z $1 ]; then
        echo "Missing python version. Select from below:"
        pyenv versions
    else
        NAME=$(basename `git rev-parse --show-toplevel`)
        echo ${NAME}
        pyenv virtualenv $1 ${NAME}
        pyenv local ${NAME}
    fi
}

## Keybindings
bindkey -e
bindkey "^[[3~" delete-char
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward

# bindkey '^W' fzf-completion
# bindkey '^I' $fzf_default_completion

# Text mods
alias comma='paste -sd "," -'
alias stripquotes="sed -e 's/^\"//' -e 's/\"$//'"

## Shell Aliases
alias load='clokta-load'
alias hd='fd ~'
alias l='exa -algF'
alias ll='exa -algF'
alias ls='exa -algF'
alias reload="exec zsh -l"
alias t='exa --tree --git-ignore'
alias tt='exa --tree --git-ignore'
alias tree='exa --tree --git-ignore'

## Dev Aliases
alias dc="docker-compose"
#alias ga="git-add"
alias gaa="git-add ."
alias gcm="git commit -m"
alias gp="git push"
alias gr="git reset"
alias gs="git status"
alias gss="git stash"
alias gsp="git stash pop"
alias gcc="git clone --recurse-submodules"
alias gl="git log --graph --decorate --pretty=oneline --abbrev-commit main origin/main temp"
alias gm="git-main"
alias num="nl"
alias linenum="nl"
alias json="jq -C | less -R"
alias avro="avro-tools"
alias ip="ifconfig | grep 'inet ' | grep -Fv 127.0.0.1 | awk '{print \$2}'"

## App aliases
alias sublime="open -a /Applications/Sublime\ Text.app"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
source ~/.p10k.zsh

function useast() {
    export AWS_REGION=us-east-1
    export AWS_DEFAULT_REGION=us-east-1
}

function eucentral() {
    export AWS_REGION=eu-central-1
    export AWS_DEFAULT_REGION=eu-central-1
}

function dw() {
    clokta --profile dw
    export AWS_PROFILE=dw
    useast
}

function bigdata() {
    clokta --profile bigdata
    export AWS_PROFILE=bigdata
    useast
}

function arc() {
    clokta --profile arc
    export AWS_PROFILE=arc
    useast
}

function arc-perso() {
    clokta --profile arc-perso
    export AWS_PROFILE=arc-perso
    useast
}

function wpit() {
    clokta --profile wpit
    export AWS_PROFILE=wpit
    useast
}

function ai() {
    clokta --profile ai
    export AWS_PROFILE=ai
    useast
}

function zeus() {
    clokta --profile zeus 
    export AWS_PROFILE=zeus
    useast
}

function zeus-sandbox() { 
    clokta --profile zeus-sandbox
    export AWS_PROFILE=zeus-sandbox
    useast
}

function colors() {
    for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$'\n'}; done
}