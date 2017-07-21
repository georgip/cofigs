
#!/bin/bash
set -x
set -e

sudo apt-get install -y vim tmux

gitconfig_path=
ln -s --interactive ${PWD}/git/gitconfig ${HOME}/.gitconfig
ln -s --interactive ${PWD}/vim/vimrc ${HOME}/.vimrc

sudo apt-get install -y emacs24 || sudo ./emacs/install_emacs.sh
mkdir -p ${HOME}/.emacs.d/
ln -s --interactive ${PWD}/emacs/init.el ${HOME}/.emacs.d/init.el

cat >> ${HOME}/.bashrc <<EOF
# Config add by configs/setup.sh
export EDITOR=vim

git_shell_config=${PWD}/git/git_shell_config
if [ -f \$git_shell_config ]; then
    . \$git_shell_config
fi

# Load python venvs
function v() {
    local name=\$1
    local path="\${HOME}/venvs/\${name}/bin/activate"

    [ -f "\${path}" ] && source \$path || echo "Python virtual environment not found: \${name}: \${path}" >&2
}
EOF

cat >> ${HOME}/.bash_aliases <<EOF
alias g='git'
alias emacs='emacs --no-window-system'
EOF
