# **************************************************
#  Instant Prompt
# **************************************************

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi


autoload -Uz compinit  && compinit

# **************************************************
#  Zsh Settings
# **************************************************

# Set emacs keybindings
set -o emacs

autoload -U colors && colors

# setopt extendedglob

# Auto-escapes pasted URLs to avoid extended globbing
autoload -U url-quote-magic bracketed-paste-magic
zle -N self-insert url-quote-magic
zle -N bracketed-paste bracketed-paste-magic

# Automatically list choices on an ambiguous completion. 
setopt auto_list


HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

setopt bang_hist 
setopt extended_history

setopt append_history  # Write to the history file immediately, not when the shell exits.
setopt share_history   # Share history between all sessions.

setopt hist_expire_dups_first  # Expire duplicate entries first when trimming history.
setopt hist_ignore_dups        # Don't record an entry that was just recorded again.
setopt hist_ignore_all_dups    # Delete old recorded entry if new entry is a duplicate.
setopt hist_find_no_dups       # Do not display a line previously found.
setopt hist_ignore_space       # Don't record an entry starting with a space.
setopt hist_save_no_dups       # Don't write duplicate entries in the history file.
setopt hist_reduce_blanks      # Remove superfluous blanks before recording entry.
setopt hist_verify             # Don't execute immediately upon history expansion.

setopt no_beep

# Loads cdr command, lets you go back to previous pwd
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

FZF_DEFAULT_COMMAND="fd --type f"

# **************************************************
#  Functions
# **************************************************

source ~/.zsh.d/functions.zsh

# **************************************************
#  Env Setup
# **************************************************

## pyenv
if command-exists pyenv; then
  eval "$(pyenv init -)"
fi

## jenv
if command-exists jenv; then
  path=(
    "${HOME}/.jenv/bin"
    $path
  )

  eval "$(jenv init -)"

  jenv enable-plugin export 1> /dev/null
  jenv enable-plugin maven 1> /dev/null
fi

## Homebrew completions
if command-exists brew; then
  fpath=(
    $fpath
    $(brew --prefix)/share/zsh/site-functions
  )
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


# **************************************************
#  Zinit
# **************************************************

ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
source "${ZINIT_HOME}/zinit.zsh"
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
    zdharma-continuum/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
    zsh-users/zsh-completions 


# **************************************************
#  zsh variables
# **************************************************
KEYTIMEOUT=1
CASE_SENSITIVE="false"

ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=80

GREP_OPTIONS="--color=auto"
CLICOLOR=1

export FZF_COMPLETION_TRIGGER='**'
export FZF_DEFAULT_OPTS='--height 40% --layout reverse'



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


alias gaa="git-add ."
alias gcm="git commit -m"
alias gp="git push"
alias gpp="git-push-branch"
alias gpl="git-pull-branch"
alias gr="git reset"
alias gs="git status"


alias sublime="open -a /Applications/Sublime\ Text.app"


# **************************************************
#  Final Setup
# **************************************************

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
### End of Zinit's installer chunk

# To customize prompt, run `p10k configure` or edit ~/dotfiles/roles/zsh/files/.p10k.zsh.
[[ ! -f ~/dotfiles/roles/zsh/files/.p10k.zsh ]] || source ~/dotfiles/roles/zsh/files/.p10k.zsh
