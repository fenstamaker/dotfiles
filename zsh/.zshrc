POWERLEVEL9K_MODE='nerdfont-complete'
source ~/powerlevel9k/powerlevel9k.zsh-theme

alias reload="source ~/.zshrc";

alias bigdata="source /Users/fenstamaker/.clokta/bigdata.sh";
alias arc="envdir ~/.envs/arc"
alias ent="envdir ~/.envs/ent"
alias syd="envdir ~/.envs/syd"
alias wpit="envdir ~/.envs/wpit"
alias hp="envdir ~/.envs/hp"
alias dw="envdir ~/.envs/dw"
alias sdbx="envdir ~/.envs/sandbox"
alias awscut="cut -d \" \" -f 3-"
alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
alias start_mongo="mongod --config /usr/local/etc/mongod.conf"
[[ -s "$HOME/.local/share/marker/marker.sh" ]] && source "$HOME/.local/share/marker/marker.sh"
alias icloud="cd ~/Library/Mobile\ Documents/com\~apple\~CloudDocs"
alias sublime="open -a /Applications/Sublime\ Text.app"

alias ungron="gron --ungron"

#alias dc="docker-compose"
#alias dcu="docker-compose up"
#alias dcb="docker-compose build"

#alias dockerclean="docker rm -v $(docker ps -a -q -f status=exited) && docker rmi $(docker images -f "dangling=true" -q)"
#alias dcc="dockerclean"
alias subtree="git pull -s subtree"

alias gcm="git commit -m"
alias cheat="tldr"
alias manuel="/usr/bin/man"
alias man="tldr"
alias json="jq"

alias java8="export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)"
alias java9="export JAVA_HOME=$(/usr/libexec/java_home -v 9)"

findreplace () { sed -i '' -- "s/$1/$2/g" * }

alias vpn="sudo openconnect --juniper --user=fenstamakerg --authgroup=TWP-main ra.washpost.com"

# alt <- moves back a word
# alt -> moves forward a word
bindkey -e
bindkey '\e\e[C' forward-word
bindkey '\e\e[D' backward-word

source /Users/fenstamaker/Developer/git-subrepo/.rc
fpath=('/Users/fenstamaker/Developer/git-subrepo/share/zsh-completion' $fpath)

shift-arrow() {
  ((REGION_ACTIVE)) || zle set-mark-command
  zle $1
}
for key kcap seq widget (
    left  LFT $'\e[1;2D' backward-char
    right RIT $'\e[1;2C' forward-char
    up    ri  $'\e[1;2A' up-line-or-history
    down  ind $'\e[1;2B' down-line-or-history
  ) {
  eval "shift-$key() shift-arrow $widget"
  zle -N shift-$key
  bindkey ${terminfo[k$kcap]-$seq} shift-$key
}
source /usr/local/bin/virtualenvwrapper.sh

kms-decrypt() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
    aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
}

eu-kms-decrypt() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
  aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext --region eu-central-1 | base64 --decode
}

arc-kms-decrypt() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
  arc aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
}

wpit-kms-decrypt() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
  wpit aws kms decrypt --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
}

syd-kms-decrypt() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
  syd aws kms decrypt --region ap-southeast-2 --ciphertext-blob fileb://<(echo "$input" | base64 --decode) --output text --query Plaintext | base64 --decode
}

pem-fingerprint() {
    if [ -z "$1" ]
    then read input
    else
        input=$1
    fi
  openssl pkcs8 -in "$input" -inform PEM -outform DER -topk8 -nocrypt | openssl sha1 -c
}

alias clavis-encrypt="arc aws kms encrypt --key-id c95f817d-6381-4b50-8647-bf8114fd24f4 --plaintext"
alias perso-encrypt="arc aws kms encrypt --key-id bec6648a-876c-4541-99f8-38c7540f996f --plaintext"
alias syd-perso-encrypt="arc aws kms encrypt --region ap-southeast-2 --key-id d8ec7d71-9d6f-4561-ab35-3fca7e3f69e5 --plaintext"

alias kmsd="kms-decrypt"

alias clokta="/Users/fenstamaker/.virtualenvs/clokta/bin/clokta"

clear-clokta() {
    unset AWS_ACCESS_KEY_ID;
    unset AWS_SECRET_ACCESS_KEY;
    unset AWS_SESSION_TOKEN;
}

git-add() {
    git add $1
    git status
}

alias ga="git-add"
alias gaa="git-add ."
alias gs="git status"
alias gr="git reset"
alias gp="git push origin"
alias gpp="git push origin master"
