# -*- mode: shell-script -*-

export ZPLUG_HOME=/opt/homebrew/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug 'dracula/zsh', as:theme
zplug 'zsh-users/zsh-syntax-highlighting', defer:2
zplug 'zsh-users/zsh-completions'
zplug 'olets/zsh-abbr'

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

export PATH=/opt/homebrew/bin/:/opt/homebrew/sbin:$PYENV_ROOT/bin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/bin:/usr/local/bin:/bin:/sbin:/usr/sbin:/usr/bin:$PATH:$HOME/Dropbox/bin:$HOME/bin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/share/google-cloud-sdk/bin/:/home/linuxbrew/.linuxbrew/bin/:
if command -v aqua 1>/dev/null 2>&1; then
    export PATH="$(aqua root-dir)/bin:$PATH"
fi

export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export LC_CTYPE=C
export EDITOR="emacs -nw -q"
export GOPATH=$HOME
export PYENV_ROOT="$HOME/.pyenv"
export BUNDLER_EDITOR="emacsclient -n"
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@3)"
export CPPFLAGS="-I/usr/local/opt/openjdk/include"
export NODE_OPTIONS="--max-old-space-size=4096"

HISTFILE=$HOME/Dropbox/dotfiles/.zsh-history
HISTSIZE=10000
SAVEHIST=100000

function history-all { \history -E 1 }

alias cp='cp -i'
alias diff='colordiff'
alias du='dust'
#alias ls='exa'
alias mv='mv -i'
alias peco='peco --layout=bottom-up'
alias ps='procs'
alias r='bundle exec rails'
alias rm='rm -i'
alias note='code ~/Dropbox/junk'
alias e='open -a /Applications/Emacs.app'
alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'

# グローバルエイリアス
alias -g G='| grep'
alias -g L='| lv -c'
alias -g H='| head'
alias -g T='| tail'
alias -g B='`git branch | peco | sed -e "s/^\*[ ]*//g"`'

if command -v hub 1>/dev/null 2>&1; then
    eval "$(hub alias -s)"
fi
if command -v mise 1>/dev/null 2>&1; then
    eval "$(mise activate zsh)"
fi

if type brew &>/dev/null; then
    FPATH=$(brew --prefix)/share/zsh-completions:$(brew --prefix)/share/zsh/site-functions:$FPATH

    autoload -Uz compinit
    compinit
fi

LOCAL_ZSH_DIR="${HOME}/Dropbox/.zsh"
# .zshがディレクトリで、読み取り、実行、が可能なとき
if [ -d $LOCAL_ZSH_DIR ] && [ -r $LOCAL_ZSH_DIR ] && [ -x $LOCAL_ZSH_DIR ]; then
    # zshディレクトリより下にある、.zshファイルの分、繰り返す
    for file in ${LOCAL_ZSH_DIR}/**/*.zsh; do
        # 読み取り可能ならば実行する
        [ -r $file ] && source $file
    done
fi

function do_enter() {
    if [ -n "$BUFFER" ]; then
        zle accept-line
        return 0
    fi
    # ls を表示
    echo
    ls -F
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

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/oyamada/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/oyamada/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/oyamada/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/oyamada/google-cloud-sdk/completion.zsh.inc'; fi

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/oyamada/.orbstack/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# pnpm
export PNPM_HOME="/Users/oyamada/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
