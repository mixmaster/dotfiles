#!/usr/bin/env zsh

rbenv_exec=$(command -v rbenv)

if [ -x "$rbenv_exec" ] ; then
  eval "$(rbenv init -)"
  export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
fi
