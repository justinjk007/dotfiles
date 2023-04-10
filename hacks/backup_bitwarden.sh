#!/bin/bash -x

# export these in .machine_config
# export BW_SESSION=""
# export MAIN_PASS=""
source ~/.machine_config > /dev/null 2>&1

if bw status | grep -q 'unlocked'; then
    bw sync
    bw export $MAIN_PASS --format json --output ./bitwarden_backup.json
    if [ -f bitwarden_backup.json ]; then
	rm -rf bitwarden_backup.7z
	mkdir bitwarden_backup
	mv bitwarden_backup.json bitwarden_backup
	# The options used are:
	#    a: Add files to archive
	#   -p: Prompt for a password
	#   -mx=9: Level of # commentmpression (9 being ultra)
	#   -mhe: Encrypt file names
	#   -t7z: Generate a 7z archive
	#   First 'bitwarden_backup' is the name of the archive
	#   second 'bitwarden_backup' is the folder to include in the archive
	7z -p$MAIN_PASS -mx=9 -mhe -t7z a bitwarden_backup bitwarden_backup
	cp bitwarden_backup.7z ~/Dropbox/Backup/
	sleep 2s
	rm -rf bitwarden_backup
	ls -l ~/Dropbox/Backup/bitwarden_backup.7z
    else
	osascript -e 'display notification "Bitwarden backup was NOT GENERATED"'
    fi
else
    osascript -e 'display notification "Bitwarden is LOCKED"'
fi
