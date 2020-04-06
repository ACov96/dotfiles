#!/bin/sh

# Install user packages
./install-packages.sh

# Archive existing configuration and link versioned configuration
./archive.sh
./link-configs.sh

echo "Configuration setup!"
