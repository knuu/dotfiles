export PATH=/usr/local/bin:$PATH

# opam
export PATH=$HOME/.opam/system/bin:$PATH
eval `opam config env`

# zsh
export ZDOTDIR=${HOME}/.zsh
source ${ZDOTDIR}/.zshenv
source ${HOME}/.zshrc

# TeX
export PATH=/usr/local/teTeX/bin:$PATH

