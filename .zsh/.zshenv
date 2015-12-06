export PATH=/usr/local/bin:$PATH

# zsh
export ZDOTDIR=${HOME}/.zsh
if [ -f ${ZDOTDIR}/.zshrc ]; then
    source ${ZDOTDIR}/.zshrc
fi

# TeX
export PATH=/usr/local/teTeX/bin:$PATH

# Emacs.app
export PATH=/Applications/Emacs.app/Contents/MacOS:$PATH

# opam
#export PATH=$HOME/.opam/system/bin:$PATH
#eval `opam config env`
