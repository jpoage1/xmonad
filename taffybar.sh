#!/usr/bin/env bash

pkill my-taffybar
# killall -q my-taffybar

launch_bar() {
	local bar=$1
    local log_file="/tmp/taffybar-$bar.log"
    echo "---" >> "$log_file"
	my-taffybar "$bar" 2>&1 | tee -a "/tmp/taffybar-$bar.log" &
	disown
}

launch_bar top
launch_bar bottom
echo "Bars launched..."
