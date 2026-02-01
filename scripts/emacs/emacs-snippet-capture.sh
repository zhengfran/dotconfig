#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Snippet Capture
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 📝
# @raycast.alias sc

# Documentation:
# @raycast.author zhengfran
# @raycast.authorURL https://raycast.com/zhengfran

emacsclient -c -e "(my/global-snippet-capture)" &>/dev/null &
