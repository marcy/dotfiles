function peco_bundle_open
  set -l query (commandline)

  if test -n $query
    set peco_flags --query "$query"
  end

  bundle show 2> /dev/null | sed -e '/^  \*/!d; s/^  \* \([^ ]*\) .*/\1/' | peco $peco_flags | read line

  if test -n $line
    echo $line
    commandline "bundle open $line"
  end
end
