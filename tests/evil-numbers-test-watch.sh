#!/bin/bash
SCRIPT_PATH="$(dirname "${BASH_SOURCE[0]}")"
echo $SCRIPT_PATH
while true; do
    inotifywait -e close_write "$SCRIPT_PATH/evil-numbers-test.el" "$SCRIPT_PATH/../evil-numbers.el"
    tput clear
    "$SCRIPT_PATH/evil-numbers-test.sh"
done
