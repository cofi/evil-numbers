#!/bin/bash
SCRIPT_PATH="$(dirname "${BASH_SOURCE[0]}")"

# `package-initialize` is needed so the 'evil' package can be found.
emacs \
    -batch \
    --eval "(package-initialize)" \
    -l $SCRIPT_PATH/evil-numbers-tests.el \
    -f ert-run-tests-batch-and-exit
