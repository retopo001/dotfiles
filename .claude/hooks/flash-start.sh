#!/bin/bash
# Flash when session starts
notify-send -u low -t 1000 "🧠 SESSION" "Claude connected" 2>/dev/null || true
echo "$(date +%s.%N) SESSION_START" >> ~/.claude/action-flash.log
