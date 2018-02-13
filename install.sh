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
ln -s "$DIR/bash_logout" "$HOME/.bash_logout"
ln -s "$DIR/bashrc" "$HOME/.bashrc"
ln -s "$DIR/profile" "$HOME/.profile"

echo "Installing 'protectSSH.sh' in $HOME/bin"
mkdir -p "$HOME/bin"
ln -s "$DIR/bin/protectSSH.sh" "$HOME/bin/protectSSH.sh"

echo "Installing .ghci"
ln -s "$DIR/haskell/ghci" "$HOME/.ghci"

echo "Setting up emacs (see $DIR/emacs/init.el)"
echo "The first time you start emacs it will install many things"
mkdir -p "$HOME/.emacs.d"
ln -s "$DIR/emacs/init.el" "$HOME/.emacs.d/init.el"

echo "Setting up tmux"
ln -s "$DIR/tmux/tmux.conf" "$HOME/.tmux.conf"

echo "Done!"
echo ""
echo "Log out/in to see the changes"
echo "(or source ~/.bashrc if you're daring)"
echo ""
echo ""
echo ""
echo " Things to know:"
echo ""
echo " - You should run `sudo projectSSH.sh` after each"
echo "   reboot to keep the OOM killer from hosing your ssh session."
echo ""
echo " - The Tmux prefix key is Ctrl-o"
echo ""
echo " - Emacs is configured with Agda-mode for .hs files."
echo "   (So some keysequences generate unicode.)"
echo ""
echo " - Emacs also is using flymake with hlint"
echo ""
echo " - Your prompt shows: "
echo "   |username on hostname @ theTime pwd (git branch)|"
echo "   <input area>"
echo ""
echo " - Non-zero exit status will also be echoed in red."
echo ""

