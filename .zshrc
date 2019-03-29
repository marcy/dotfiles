# -*- mode: shell-script -*-

export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug 'dracula/zsh', as:theme
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-completions'

# 未インストール項目をインストールする
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose

setopt hist_expand           # 補完時にヒストリを自動的に展開
setopt hist_ignore_all_dups  # ヒストリに追加されるコマンド行が古いものと同じなら古いものを削除
setopt hist_ignore_dups      # 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_space     # スペースで始まるコマンド行はヒストリリストから削除
setopt hist_no_store         # historyコマンドは履歴に登録しない
setopt hist_reduce_blanks    # 余分な空白は詰めて記録
setopt hist_save_no_dups     # 古いコマンドと同じものは無視
setopt hist_verify           # ヒストリを呼び出してから実行する間に一旦編集可能
setopt inc_append_history    # 履歴をインクリメンタルに追加
setopt magic_equal_subst     # --prefix=の後のパス名を補完したり
setopt share_history         # ヒストリの共有の有効化

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない

export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export LC_CTYPE=C
export EDITOR="emacs -nw -q"
export GOPATH=$HOME
export PYENV_ROOT="$HOME/.pyenv"
export PATH=$PYENV_ROOT/bin:$HOME/.cabal/bin:$HOME/bin:/opt/local/bin:/usr/local/bin:/bin:/usr/local/sbin:/sbin:/usr/sbin:/usr/bin:/usr/X11R6/bin:$PATH:$JRUBY_HOME/bin:$HOME/Dropbox/bin:/usr/local/share/npm/bin:$GOPATH/bin:/usr/local/share/git-core/contrib/diff-highlight:/usr/local/opt/mysql@5.6/bin
export BUNDLER_EDITOR="emacsclient -n"

HISTFILE=$HOME/Dropbox/dotfiles/.zsh-history
HISTSIZE=10000
SAVEHIST=100000

function history-all { \history -E 1 }

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ls='ls -Fv'
alias peco='peco --layout=bottom-up'
alias diff='colordiff'
alias r='bundle exec rails'
alias be='bundle exec'
alias g="git"
alias ec='emacsclient -n'
alias e='open -a /Applications/Emacs.app'

# グローバルエイリアス
alias -g G='| grep'
alias -g L='| lv -c'
alias -g H='| head'
alias -g T='| tail'
alias -g B='`git branch | peco | sed -e "s/^\*[ ]*//g"`'

eval "$(rbenv init - zsh)"
eval "$(hub alias -s)"

eval "$(pyenv init -)"

function do_enter() {
    if [ -n "$BUFFER" ]; then
        zle accept-line
        return 0
    fi
    # ls を表示
    echo
    ls
    # git status を表示
    # ls_abbrev
    if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
        echo -e "\e[0;33m--- git status ---\e[0m"
        git status -sb
    fi
    zle reset-prompt
    return 0
}
zle -N do_enter
bindkey '^m' do_enter

# peco hitory
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    #zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

# peco git-project
function peco-cd-git-project() {
    cd $(ghq list -p | peco)
    zle accept-line
}
zle -N peco-cd-git-project
bindkey '^]' peco-cd-git-project

# peco bundle open
function peco-bundle-open() {
    local selected=$(bundle show 2> /dev/null | sed -e '/^  \*/!d; s/^  \* \([^ ]*\) .*/\1/' | peco --query "$LBUFFER")
    if [ -n "$selected" ]; then
        BUFFER="bundle open ${selected}"
        zle accept-line
    fi
    #zle clear-screen
}
zle -N peco-bundle-open
bindkey '^o' peco-bundle-open
