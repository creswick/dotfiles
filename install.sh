#!/bin/bash

# Unofficial strict mode: (here for reference as much as to help)
set -euo pipefail
IFS=$'\n\t'


# The directory of this script; taken from stack overflow.
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

echo "Installing dot file content from: $DIR"



cd "$HOME"

#
# This really should be cleaner, and use a bash function / map
# of link to file but I'm tired.
#

echo "Installing symlinks for bash"
ln -s -f  "$DIR/bash_logout" "$HOME/.bash_logout"
ln -s -f "$DIR/bashrc" "$HOME/.bashrc"
ln -s -f "$DIR/profile" "$HOME/.profile"

echo "Installing 'protectSSH.sh' in $HOME/bin"
mkdir -p "$HOME/bin"
ln -s -f "$DIR/bin/protectSSH.sh" "$HOME/bin/protectSSH.sh"

echo "Installing .ghci"
ln -s -f "$DIR/haskell/ghci" "$HOME/.ghci"

echo "Setting up emacs (see $DIR/emacs/init.el)"
echo "The first time you start emacs it will install many things"
mkdir -p "$HOME/.emacs.d"
ln -s -f "$DIR/emacs/init.el" "$HOME/.emacs.d/init.el"

echo "Setting up tmux"
ln -s -f "$DIR/tmux/tmux.conf" "$HOME/.tmux.conf"

echo "Done!"
echo ""
echo "Log out/in to see the changes"
echo "(or source ~/.bashrc if you're daring)"
echo ""
echo ""
cat "$DIR/thingsToKnow.txt"

