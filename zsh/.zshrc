
# uautoload -Uz compinit
# compinit

if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

alias java8="export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)"
alias java11="export JAVA_HOME=$(/usr/libexec/java_home -v 11)"
export WORKON_HOME=~/.venv
export PIPENV_VENV_IN_PROJECT=1

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

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

antigen bundle hlissner/zsh-autopair
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zdharma/fast-syntax-highlighting
antigen bundle changyuheng/zsh-interactive-cd
antigen bundle wfxr/forgit

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
# source $HOME/.zsh.d/plugins/powerlevel9k/powerlevel9k.zsh-theme

## Plugins
# source $HOME/.zsh.d/plugins/zsh-autopair/autopair.zsh
# source $HOME/.zsh.d/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# source $HOME/.zsh.d/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
# source $HOME/.zsh.d/plugins/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh
# source $HOME/.zsh.d/plugins/forgit/forzsh-interactive-cd.plugin.zshgit.plugin.zsh

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

## Powerlevel

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
    user
    dir
    virtualenv
    vcs
)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
    status
    root_indicator
    background_jobs
    history
    command_execution_time
    time
)

POWERLEVEL9K_VCS_GIT_HOOKS=(
    vcs-detect-changes
    git-aheadbehind
    git-remotebranch
)

POWERLEVEL9K_PROMPT_ON_NEWLINE=true

## Functions
git-add() {
    git add $1
    git status
}

git-clone() {
    git clone $1
    git submodule update --init
}

git-branch-from-master() {
    if [ -z $1 ]; then
        echo "Missing branch name"
        exit 1
    fi
    git checkout master
    git pull origin master
    git checkout -b $1
}

gpp() {
    BRANCH=$1
    if [ -z $BRANCH ]; then
        BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
    fi
    git push origin $BRANCH
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

# load() {
#     ENV_FILE=$1
#     shift
#     CMD="$@"
#     FILE="${HOME}/.envs/.${ENV_FILE}"
#     if [ ! -f $FILE ]; then
#         FILE="${HOME}/.clokta/${ENV_FILE}.env"
#         if [ ! -f $FILE ]; then
#             echo "Could not file env file"
#             return 1
#         fi
#     fi

#     SCRIPT="source ${HOME}/.zshrc; source ${FILE} || exit 1; $CMD"
#     zsh -ac $SCRIPT
#     return 0
# }

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
unalias z
z() {
  if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _last_z_args="$@"
    _z "$@"
  fi
}

zz() {
  cd "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q "$_last_z_args")"
}

lower() {
    echo "$1" | tr '[:upper:]' '[:lower:]'
}

upper() {
    echo "$1" | tr '[:lower:]' '[:upper:]'
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
alias gcc="git clone --recurse-submodules"
alias gl="git log --graph --decorate --pretty=oneline --abbrev-commit master origin/master temp"
alias gbm="git-branch-from-master"
alias num="nl"
alias linenum="nl"
alias json="jq -C | less -R"
alias avro="avro-tools"
alias ip="ifconfig | grep 'inet ' | grep -Fv 127.0.0.1 | awk '{print \$2}'"

## App aliases
alias sublime="open -a /Applications/Sublime\ Text.app"

## Dockerapps
# alias aws="docker run -it --rm -v "${HOME}/.aws:/root/.aws" --log-driver none --name aws awscli"

# alias bigdata="source /Users/fenstamaker/.clokta/bigdata.sh";
# alias arc="envdir ~/.envs/arc"
# alias ent="envdir ~/.envs/ent"
# alias syd="envdir ~/.envs/syd"
# alias wpit="envdir ~/.envs/wpit"
# alias hp="envdir ~/.envs/hp"
# alias dw="envdir ~/.envs/dw"
# alias sdbx="envdir ~/.envs/sandbox"
# alias awscut="cut -d \" \" -f 3-"
# alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
# alias start_mongo="mongod --config /usr/local/etc/mongod.conf"
# [[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"
# alias icloud="cd ~/Library/Mobile\ Documents/com\~apple\~CloudDocs"
# alias sublime="open -a /Applications/Sublime\ Text.app"

# alias ungron="gron --ungron"

# #alias dcu="docker-compose up"
# #alias dcb="docker-compose build"

# #alias dockerclean="docker rm -v $(docker ps -a -q -f status=exited) && docker rmi $(docker images -f "dangling=true" -q)"
# #alias dcc="dockerclean"
# alias subtree="git pull -s subtree"

# alias gcm="git commit -m"
# alias cheat="tldr"
# alias manuel="/usr/bin/man"
# alias man="tldr"
# alias json="jq"

# findreplace () { sed -i '' -- "s/$1/$2/g" * }

# alias vpn="sudo openconnect --juniper --user=fenstamakerg --authgroup=TWP-main ra.washpost.com"

# # alt <- moves back a word
# # alt -> moves forward a word
# bindkey -e
# bindkey '\e\e[C' forward-word
# bindkey '\e\e[D' backward-word

# source /Users/fenstamaker/Developer/git-subrepo/.rc
# fpath=('/Users/fenstamaker/Developer/git-subrepo/share/zsh-completion' $fpath)

# shift-arrow() {
#   ((REGION_ACTIVE)) || zle set-mark-command
#   zle $1
# }
# for key kcap seq widget (
#     left  LFT $'\e[1;2D' backward-char
#     right RIT $'\e[1;2C' forward-char
#     up    ri  $'\e[1;2A' up-line-or-history
#     down  ind $'\e[1;2B' down-line-or-history
#   ) {
#   eval "shift-$key() shift-arrow $widget"
#   zle -N shift-$key
#   bindkey ${terminfo[k$kcap]-$seq} shift-$key
# }
# source /usr/local/bin/virtualenvwrapper.sh
# kms-decrypt() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#     aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
# }

# eu-kms-decrypt() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#   aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext --region eu-central-1 | base64 --decode
# }

# arc-kms-decrypt() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#   arc aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
# }

# wpit-kms-decrypt() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#   wpit aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
# }

# syd-kms-decrypt() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#   syd aws kms decrypt --region ap-southeast-2 --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
# }

# pem-fingerprint() {
#     if [ -z "$1" ]
#     then read input
#     else
#         input=$1
#     fi
#   openssl pkcs8 -in "$input" -inform PEM -outform DER -topk8 -nocrypt | openssl sha1 -c
# }

# alias clavis-encrypt="arc aws kms encrypt --key-id c95f817d-6381-4b50-8647-bf8114fd24f4 --plaintext"
# alias perso-encrypt="arc aws kms encrypt --key-id bec6648a-876c-4541-99f8-38c7540f996f --plaintext"
# alias syd-perso-encrypt="arc aws kms encrypt --region ap-southeast-2 --key-id d8ec7d71-9d6f-4561-ab35-3fca7e3f69e5 --plaintext"

# alias kmsd="kms-decrypt"

# alias clokta="/Users/fenstamaker/.virtualenvs/clokta/bin/clokta"

# clear-clokta() {
#     unset AWS_ACCESS_KEY_ID;
#     unset AWS_SECRET_ACCESS_KEY;
#     unset AWS_SESSION_TOKEN;
# }
