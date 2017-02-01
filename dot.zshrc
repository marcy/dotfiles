export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug 'chrissicool/zsh-256color', use:zsh-256color.plugin.zsh
zplug 'dracula/zsh', as:theme

zplug 'marcy/dotfiles', use:local.zsh

# 未インストール項目をインストールする
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose
