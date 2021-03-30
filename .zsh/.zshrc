# 環境変数
export LANG=ja_JP.UTF-8

# ===== zsh customize =====

## additional packages

### zsh-completions
fpath=(/usr/local/share/zsh-completions/(N-/) $fpath)

### zsh-syntax-highlighting
autoload -Uz colors
colors
[[ -f /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

### zsh-git-prompt
source "/usr/local/opt/zsh-git-prompt/zshrc.sh"

## configs

### ls color
export LSCOLORS=gxfxxxxxcxxxxxxxxxgxgx
export LS_COLORS='di=01;36:ln=01;35:ex=01;32'
zstyle ':completion:*' list-colors 'di=36' 'ln=35' 'ex=32'

setopt SHARE_HISTORY
setopt AUTO_CD

setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS

### cdr command
autoload -Uz add-zsh-hook
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 200
zstyle ':chpwd:*' recent-dirs-default true

bindkey -e # Emacs keybind

### C-w for word splitting
autoload -Uz select-word-style
select-word-style default
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

### undo & redo
bindkey "^_" undo
bindkey "^[_" redo

### prompt settings
PROMPT="%{${fg[red]}%}[%n](%*%)%{${reset_color}%} %#~ "
RPROMPT="%{${fg[red]}%}[%~]%{${reset_color}%} $(git_super_status)"

setopt IGNORE_EOF # Do not exit by ^D
setopt NO_FLOW_CONTROL # disable ^Q/^S
setopt NO_BEEP # disable beep

### 補完機能を有効にする
autoload -Uz compinit
compinit -u

### メニュー選択モードを有効化
zstyle ':completion:*:default' menu select=2

### 大文字と小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

### history settings
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

### インクリメンタルな検索を有効化
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

### 既に入力した内容を使ってコマンド履歴を検索する
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
bindkey '^o' history-beginning-search-backward-end

setopt print_eight_bit  # enable to show 8bit chars (e.g. Japanese)
setopt nonomatch  # enable regexp

# ===== alias =====
alias ls='ls -GF'
alias la='ls -a'
alias ll='ls -l'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias mkdir='mkdir -p'
alias pdf='open -a Preview'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
#alias -g N='> /dev/null'
#alias -g V='| vim -R -'
alias -g P=' --help | less'

# ===== Python setting =====

## vertualenv
export WORKON_HOME=$HOME/.virtualenvs
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
fi

## pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

## pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
## pip zsh completion end

# ===== Other Software Setting =====

## gcc (for competitive programming, need to symbolic link to gcc-XX)
alias gcc='gcc -O2'
alias g++='g++ -std=gnu++2a -Wall -Wextra -Wfloat-equal -Winit-self -Wlogical-op -D_GLIBCXX_DEBUG -O2'

## parscit
alias parscit='~/Library/parscit/bin/citeExtract.pl'

## jakld
alias jakld='java -jar jakld.jar'

## Stanford NLP
#export STANFORD_PATH=$HOME/Library/Stanford_NLP
#export PATH=$STANFORD_PATH/stanford-ner-2015-04-20:$STANFORD_PATH/stanford-postagger-full-2015-04-20:$STANFORD_PATH/stanford-parser-full-2015-04-20:$PATH
#export STANFORD_MODELS=$STANFORD_PATH/stanford-postagger-full-2015-04-20/models:$STANFORD_PATH/stanford-ner-2015-04-20/classifier

# topcoder
alias topcoder='open ~/topcoder/ContestAppletProd.jnlp'

# nimble
export PATH=$HOME/.nimble/bin:$PATH

# pandoc_pdf
alias pandoc2pdf='pandoc -V documentclass=ltjsarticle --pdf-engine=lualatex -V geometry:margin=1in'

# js for mac
if [[ "$OSTYPE" =~ darwin ]];then
    jscpath="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources"
    if [ -f $jscpath/jsc ];then
        export PATH=$PATH:$jscpath
    fi
fi
