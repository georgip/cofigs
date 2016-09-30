#!/bin/bash
set -x
set -e

sudo apt-get install -y vim tmux
ln -s ${PWD}/git/gitconfig ${HOME}/.gitconfig
ln -s ${PWD}/vim/vimrc ${HOME}/.vimrc

sudo ./emacs/install_emacs.sh
mkdir -p ${HOME}/.emacs.d/
ln -s ${PWD}/emacs/init.el ${HOME}/.emacs.d/init.el

cat >> ${HOME}/.bashrc <<EOF

alias g='git'
alias emacs='emacs -nw'

export EDITOR=vim
git_shell_config=${PWD}/git/git_shell_config
if [ -f \$git_shell_config ]; then
    . \$git_shell_config
fi
EOF
