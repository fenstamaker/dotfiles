#!/bin/bash

link_brew_pyenv() {
  rm -f "$HOME/.pyenv/versions/*-brew"

  for i in $(brew --cellar python)/*; do
    ln -s $i $HOME/.pyenv/versions/${i}-brew;
  done

  for i in $(brew --cellar python@2)/*; do
    ln -s $i $HOME/.pyenv/versions/${i}-brew;
  done
}