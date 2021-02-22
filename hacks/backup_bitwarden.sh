#!/bin/bash

MASTER_PASS=myPAssword123

bw export $MASTER_PASS --format encrypted_json --output ./bitwarden_backup_encrypted.json
cp bitwarden_backup_encrypted.json ~/Dropbox/
sleep 10s
rm bitwarden_backup_encrypted.json
