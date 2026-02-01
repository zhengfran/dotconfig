#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Snippet Search
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🔍
# @raycast.alias ss

# Documentation:
# @raycast.author zhengfran
# @raycast.authorURL https://raycast.com/zhengfran

emacsclient -c -e "(my/global-snippet-search)" &>/dev/null &
