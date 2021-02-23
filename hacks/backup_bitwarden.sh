#!/bin/bash -x

/usr/local/bin/bw export $MASTER_PASS --format encrypted_json --output ./bitwarden_backup_encrypted.json

if [ -f bitwarden_backup_encrypted.json ]; then
    cp bitwarden_backup_encrypted.json ~/Dropbox/
    sleep 2s
    rm -rf bitwarden_backup_encrypted.json
    ls -l ~/Dropbox/bitwarden_backup_encrypted.json
else
    /usr/bin/osascript -e 'display notification "Bitwarden backup was NOT GENERATED"'
fi

