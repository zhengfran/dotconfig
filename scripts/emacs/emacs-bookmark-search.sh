#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Bookmark Search
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🔖
# @raycast.alias bs

# Documentation:
# @raycast.author zhengfran
# @raycast.authorURL https://raycast.com/zhengfran

emacsclient -c -e "(my/global-bookmark-search)" &>/dev/null &
