#!/bin/bash

bw export $MASTER_PASS --format encrypted_json --output ./bitwarden_backup_encrypted.json
cp bitwarden_backup_encrypted.json ~/Dropbox/
sleep 5s
rm -rf bitwarden_backup_encrypted.json
