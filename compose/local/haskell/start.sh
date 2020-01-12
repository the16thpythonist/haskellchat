#!/bin/sh

set -o errexit
set -o nounset

# Actually starting the haskell server application
printf "STARTING THE SERVER...\n"

stack build
chown -R root:root /app/.stack-work
stack exec haskellchat
#stack exec -- yesod devel