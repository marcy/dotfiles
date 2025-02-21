# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Customize Oh My Fish configuration path.
set -gx OMF_CONFIG "$HOME/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

set -Ux GOPATH $HOME/workspace/go

set -U fish_user_paths $fish_user_paths $GOPATH/bin

set -Ux LC_ALL ja_JP.UTF-8

set -Ux EDITOR 'emacsclient -n'

alias mv 'mv -i'
alias cp 'cp -i'
alias rm 'rm -i'

alias g git

eval (hub alias -s)

function done_enter --on-event fish_postexec
    if test -z "$argv"
        ls
        if git rev-parse --is-inside-work-tree ^/dev/null
            echo (set_color yellow)"--- git status ---"(set_color normal)
            git status -sb
        end
    end
end

function fish_user_key_bindings
  bind \c] peco_select_ghq_repository
  bind \ct peco_select_zsh_history
  bind \cr peco_select_history
  bind \co peco_bundle_open
end

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
set --export --prepend PATH "/Users/oyamada/.rd/bin"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
