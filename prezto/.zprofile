#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='nano'
export VISUAL='emacs -nw'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  /usr/local/{bin,sbin}
  $HOME/bin
  $HOME/.local/bin
  $HOME/.pyenv/bin
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi


#
# pyenv
#
if [[ -s "$HOME/.pyenv" ]]; then
    export pyenv_root="$HOME/.pyenv"
    eval "$(pyenv init - --no-rehash)"
fi

#
# nvm
#
if [[ -s "$HOME/.nvm" ]]; then
    export NVM_DIR="$HOME/.nvm"
    \. "$NVM_DIR/nvm.sh"
fi

#
# poetry
#
if [[ -s "$HOME/.poetry" ]]; then
    export PATH="$HOME/.poetry/bin:$PATH"
fi

#
# spark
#
if [[ -s "$HOME/spark" ]]; then
    export PATH="$HOME/spark/bin:$PATH"
fi

#
# ARM brew
#
if [[ -s "/opt/homebrew/bin" ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
    alias ibrew="arch -x86_64 /usr/local/bin/brew"
fi

#
# for gcloud
#
if [[ -s "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk" ]]; then
    source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
fi

#
# for go bin
#
if [[ -s "$HOME/go/bin" ]]; then
    export PATH="$PATH:$HOME/go/bin"
fi

#
# for dotnet bin
#
if [[ -s "$HOME/.dotnet/tools" ]]; then
    export PATH="$PATH:$HOME/.dotnet/tools"
fi

#
# for ghcup
#
if [[ -f "/Users/okoks9011/.ghcup/env" ]]; then
    source "/Users/okoks9011/.ghcup/env" # ghcup-env
fi

#
# for cargo
#
if [[ -f "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

#
# for flutter
#
if [[ -s "$HOME/flutter/bin" ]]; then
    export PATH="$PATH:$HOME/flutter/bin"
fi
