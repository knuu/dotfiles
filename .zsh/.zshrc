# 環境変数
export LANG=ja_JP.UTF-8

# zsh-completions
fpath=(/usr/local/share/zsh-completions/(N-/) $fpath)
# 色を使用出来るようにする
autoload -Uz colors
colors
[[ -f /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

setopt SHARE_HISTORY
setopt AUTO_CD


setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

# cdrコマンド
autoload -Uz add-zsh-hook
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 200
zstyle ':chpwd:*' recent-dirs-default true

# Emacs風のキーバインドにする
bindkey -e

# C-wの単語の区切り設定
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# undo, redo
bindkey "^_" undo
bindkey "^[_" redo

# プロンプトの設定
PROMPT="%{${fg[red]}%}[%n](%*%)%{${reset_color}%} %#~ "
RPROMPT="%{${fg[red]}%}[%~]%{${reset_color}%}"

# ^Dでzshを終了しない
setopt IGNORE_EOF
# ^Q/^Sのフローコントロールを無効化
setopt NO_FLOW_CONTROL
# ビープ音を鳴らさない
setopt NO_BEEP

# 補完機能を有効にする
autoload -Uz compinit
compinit -u

# メニュー選択モードを有効化
zstyle ':completion:*:default' menu select=2

# 大文字と小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# コマンド履歴の保存
HISTFILE=~/.zsh_history
HIST_SIZE=1000000
SAVE_HIST=1000000

# インクリメンタルな検索を有効化
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

# 既に入力した内容を使ってコマンド履歴を検索する
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
bindkey '^o' history-beginning-search-backward-end

# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# エイリアス
alias ls='ls -F'
alias la='ls -a'
alias ll='ls -l'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'
alias pdf='open -a Preview'
alias ltc='/bin/sh ~/latex.sh'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
#alias -g N='> /dev/null'
#alias -g V='| vim -R -'
alias -g P=' --help | less'

## vertualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

## rbenv
export PATH=$HOME/.rbenv/bin:$PATH
eval "$(rbenv init -)"

## pyenv
export PYENV_ROOT=/usr/local/opt/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

## gcc-5
alias gcc='gcc-5 -O2'
alias g++='g++-5 -std=c++11 -Wall -Wextra -Wfloat-equal -Winit-self -Wlogical-op -D_GLIBCXX_DEBUG -O2'

## parscit
alias parscit='~/Library/parscit/bin/citeExtract.pl'

## jakld
alias jakld='java -jar jakld.jar'

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

## Stanford NLP
export STANFORD_PATH=$HOME/Library/Stanford_NLP
export PATH=$STANFORD_PATH/stanford-ner-2015-04-20:$STANFORD_PATH/stanford-postagger-full-2015-04-20:$STANFORD_PATH/stanford-parser-full-2015-04-20:$PATH
export STANFORD_MODELS=$STANFORD_PATH/stanford-postagger-full-2015-04-20/models:$STANFORD_PATH/stanford-ner-2015-04-20/classifier

# topcoder
alias topcoder='open ~/topcoder/ContestAppletProd.jnlp'

# nimble
export PATH=$HOME/.nimble/bin:$PATH
