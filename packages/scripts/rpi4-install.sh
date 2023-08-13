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
pass nixosRpi4_ssh_host_ed25519_key > "$temp/etc/ssh/ssh_host_ed25519_key"

# Set the correct permissions so sshd will accept the key
chmod 600 "$temp/etc/ssh/ssh_host_ed25519_key"

install -d -m755 "$temp/boot"

pass nixosRpi4_ssh_initrd_ed25519_key > "$temp/boot/initrd-ssh-key"

# Intall rashberry pi uefi firmware
cp -r --no-preserve="all" rpi4-uefi-firmware/* "$temp/boot/"

# Call nixos-anywhere with disk encryption keys and ssh hostek
nixos-anywhere \
  --disk-encryption-keys /tmp/secret.key <(echo -n $(pass nixosRpi4Encryption)) \
  --extra-files "$temp" \
  --flake '.#nixosRpi4' \
  --build-on-remote \
  --debug \
  --option show-trace true \
  root@nixosRpi4IP
