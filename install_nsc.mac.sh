#!/bin/sh

# Dependency Checks

echo "Running dependency checks for installation"
DEP_ERR_MSG="must be installed before installing nsc"

if ! command -v stack; then
    echo "The Haskell tool 'Stack' $DEP_ERR_MSG"
    exit 1
fi

stack install

mv "$(stack path --local-bin)/no-syn-exe" /usr/local/bin/nsc

echo "Install complete"
