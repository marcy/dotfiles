set ZSH_HISTORY_FILE $HOME/Dropbox/dotfiles/dot.zsh-history

function peco_select_zsh_history
  set -l query (commandline)

  if test -n $query
    set peco_flags --query "$query"
  end

  tail -n10000 $ZSH_HISTORY_FILE | nkf -w | cut -d ';' -f 2- | peco $peco_flags | read line

  if [ $line ]
    commandline $line
  else
    commandline -f repaint
  end
end
