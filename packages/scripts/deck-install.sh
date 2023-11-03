#!/usr/bin/env bash

# Call nixos-anywhere with disk encryption keys and ssh hostek
# --disk-encryption-keys /tmp/secret.key <(echo -n $(pass nixosDeckEncryption)) \

nixos-anywhere \
  --flake '.#nixosDeck' \
  root@nixosDeckIP
