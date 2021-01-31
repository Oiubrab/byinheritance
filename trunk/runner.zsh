#!/bin/zsh

# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
# Use manjaro zsh prompt
if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  source /usr/share/zsh/manjaro-zsh-prompt
fi

# Source the opencoarrays package
source /home/avltbyzn/thermonuclear/OpenCoarrays-2.9.2/prerequisites/installations//opencoarrays/2.9.2/setup.sh

# Run the damn program
cafrun -n 16 --use-hwthread-cpus in_search_of_sanity $1 $2 $3 $4

# Remind me what we did
echo $1 $2 $3 $4

exit
