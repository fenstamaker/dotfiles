# **************************************************
#  Core Functions
# **************************************************

function command-exists() {
  COMMAND=$1
  command -v ${COMMAND} 1>/dev/null 2>&1
  return $?
}

function load-if-exists() {
  FILE=$1
  [ -f ${FILE} ] && source ${FILE}
}


# **************************************************
#  Git Functions
# **************************************************

## Show what's been added
function git-add() {
  git add $1
  git status
}

## Creates a new branch off of main or master
function git-branch-from-main() {
  if [ -z $1 ]; then
    echo "Missing branch name"
    exit 1
  fi

  local branch
  local main_exists=$(git ls-remote --heads origin main)

  if [ -z $main_exists ]; then
    branch="main"
  else
    branch="master"
  fi

  git checkout $branch
  git pull origin $branch
  git checkout -b $1
}

## If no branch is given, pulls the current branch
function git-pull-branch() {
  local branch=$1
  if [ -z $branch ]; then
    branch=$(git branch | grep \* | cut -d ' ' -f2)
  fi
  git pull origin $branch
}

## If no branch is given, pushes the current branch
function git-push-branch() {
  local branch=$1
  if [ -z $branch ]; then
    branch=$(git branch | grep \* | cut -d ' ' -f2)
  fi
  git push origin $branch
}

## Clones and fetches submodules
function git-submodule-clone() {
  git clone $1
  git submodule update --init
}



# **************************************************
#  Navigation Functions
# **************************************************

alias zfzf="fzf --layout reverse --height 40%"

function dotfiles() {
  cd "${HOME}/dotfiles"
}

# Find text in project
function fg() {
  local file
  local line
  read -r file line <<< $(
    ag --nobreak --noheading $@ | 
    fzf --border -0 -1 | 
    awk -F: '{print $1, $2}'
  )

  echo $file:$line
}

## cd to a folder in ~/Projects
function pd() {
  local dir
  dir=$(exa -aD ~/Projects | zfzf)
  cd "${HOME}/Projects/${dir}"
}

## Fuzzy cd, searches through all directories recursively
function zd() {
  local dir
  dir=$(fd ${1:-.} --type d 2> /dev/null | zfzf) 
  cd "${dir}"
}

## Find file and cd into containing dir
function zdf() {
  local file
  local dir
  file=$(fzf +m -q "$1")
  dir=$(dirname "$file")
  cd "$dir"
}

## Interactive cd for parent directories
function zdr() {
  local declare dirs=()
  get_parent_dirs() {
    if [[ -d "${1}" ]]; then dirs+=("$1"); else return; fi
    if [[ "${1}" == '/' ]]; then
      for _dir in "${dirs[@]}"; do echo $_dir; done
    else
      get_parent_dirs $(dirname "$1")
    fi
  }
  local dir=$(get_parent_dirs $(realpath "${1:-$PWD}") | zfzf)
  cd "${dir}"
}



# **************************************************
#  Hooks
# **************************************************

# Excludes certain commands from history
# rm - Could include sensitve files
# echo - Could include passwords/keys
function zshaddhistory() {
  emulate -L zsh
  if [[ $1 =~ "\brm\b|^echo|\bf\b" ]] ; then
    return 1
  fi
}


# **************************************************
#  Utilities
# **************************************************

function colors() {
    for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$'\n'}; done
}