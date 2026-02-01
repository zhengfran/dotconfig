#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Bookmark Capture
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 📌
# @raycast.alias bc

# Documentation:
# @raycast.author zhengfran
# @raycast.authorURL https://raycast.com/zhengfran

emacsclient -c -e "(my/global-bookmark-capture)" &>/dev/null &
