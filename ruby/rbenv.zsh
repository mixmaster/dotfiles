#!/usr/bin/env zsh

rbenv_exec=$(command -v rbenv)
brew_exec=$(command -v brew)

if [ -x "$rbenv_exec" ] ; then
  eval "$(rbenv init -)"
  if [ -x "$brew_exec" ] ; then
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
  fi
fi
