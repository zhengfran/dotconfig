#!/bin/bash
# screen_width=$(aerospace-list-monitors --focuse wjd | jq .frame.width)
# didn't find out how to get monitor width with aerospace for now,hard code it
monitor_id=$(aerospace list-monitors --focused | awk '{print $1}')
# echo $monitor_id
read width height < <(
  displayplacer list | awk -v sid="$screen_id" '
    $0 ~ "Contextual screen id: " sid { found=1; next }
    found && /Resolution:/ {
      split($2, dims, "x")
      print dims[1], dims[2]
      exit
    }'
)
# echo $width
# echo $height
golden_width=$(echo "$width * 0.618" | bc | cut -d'.' -f1)
# echo $golden_width
# Assuming full height (adjust if you want)
aerospace resize smart "$golden_width"
