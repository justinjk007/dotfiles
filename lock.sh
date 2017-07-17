#!/bin/bash
scrot /tmp/screenshot.png
convert /tmp/screenshot.png -blur 0x5 /tmp/screenshotblur.png
i3lock -i /tmp/screenshotblur.png

# Install by
# sudo apt-get install i3lock scrot imagemagick xautolock
# File should be /bin/lock
# sudo chmod +x /bin/lock --> Make it into an executable
