#!/bin/sh

# Setup my Arch Linux rice and link my dotfiles correctly

# Install user packages
./install-packages.sh

# Archive existing configuration and link versioned configuration
./archive.sh
./link-configs.sh

echo "Configuration setup!"
