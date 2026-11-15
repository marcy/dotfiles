#!/bin/sh
# Syntax-check the shell configs in this repository.
# Tools that are not installed are skipped, so this also runs locally.
set -u
status=0

run_check() {
  tool=$1
  shift
  if command -v "$tool" >/dev/null 2>&1; then
    echo "== $*"
    "$@" || status=1
  else
    echo "skip: $tool not installed"
  fi
}

run_check zsh zsh -n .zshrc

for f in .config/fish/config.fish .config/fish/functions/*.fish; do
  [ -e "$f" ] || continue
  run_check fish fish --no-execute "$f"
done

exit $status
