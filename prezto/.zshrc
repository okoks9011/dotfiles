#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# alias
alias e='emacs -nw'
alias cabal='TERM=dumb cabal'

# settings for TRAMP
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '
