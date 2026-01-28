#!/bin/bash
# Log every user prompt with timestamp
DATA=$(cat)
PROMPT=$(echo "$DATA" | jq -r '.prompt // "" | .[0:100]')
echo "$(date +%s.%N) PROMPT: $PROMPT" >> ~/.claude/action-flash.log
