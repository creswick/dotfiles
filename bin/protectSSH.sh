#!/bin/bash
# This helps prevent SSHD from being killed by the OOM killer
# Needs to be run by root, after each boot.
if [ -e /var/run/sshd.pid ]; then
  echo "Setting the oom properties for sshd"
  echo -15 > /proc/`cat /var/run/sshd.pid`/oom_adj
  echo -1000 > /proc/`cat /var/run/sshd.pid`/oom_score_adj
fi
