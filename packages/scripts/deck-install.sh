#!/usr/bin/env bash

# Create a temporary directory
temp=$(mktemp -d)

# Function to cleanup temporary directory on exit
cleanup() {
  rm -rf "$temp"
}
trap cleanup EXIT

# Create the directory where sshd expects to find the host keys
install -d -m755 "$temp/etc/ssh"

# Decrypt your private key from the password store and copy it to the temporary directory
# generated with
# ssh-keygen
# cat flosDeck_ssh_host_ed25519_key | wl-copy
# pass insert -m flosDeck_ssh_host_ed25519_key_key
pass flosDeck_ssh_host_ed25519_key > "$temp/etc/ssh/ssh_host_ed25519_key"

# Set the correct permissions so sshd will accept the key
chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"

nixos-anywhere \
  --disk-encryption-keys /tmp/secret.key <(echo -n $(pass nixosDeckEncryption)) \
  --extra-files "$temp" \
  --flake '.#flosDeck' \
  root@nixosDeckIP
