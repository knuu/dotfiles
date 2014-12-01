export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

export PS1="\[\e[31;1m\][\w]\$\[\e[m\] " # coloring
export PGDATA=/usr/local/var/postgres

export HISTSIZE=100000 # これだけコマンド履歴を残す
export LANG='ja_JP.UTF-8' # 以下 3 つ文字コード
export LC_ALL='ja_JP.UTF-8'
export LC_MESSAGES='ja_JP.UTF-8'
