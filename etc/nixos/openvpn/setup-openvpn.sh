#!/usr/bin/env bash

set -e

# https://stackoverflow.com/a/29613573
quoteSubst() {
  IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$1")
  printf %s "${REPLY%$'\n'}"
}

echo -n "Output directory for [/etc/openvpn]: "
read -r output
if [ -z "$output" ]; then
    output="/etc/openvpn"
fi
output="$(realpath "$output")"

if [ -e "$output" ]; then
    echo "Output directory already exists"
    [ "$1" != "reuse" ] && exit
else
    file="$(mktemp)"
    wget "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip" -O "$file"
    unzip -q "$file" -d "$output"
fi

pushd "$output" > /dev/null

echo -n "Configure with automatic login? [Y/n] "
read -r autologinstr
autologin="false"
if [ -z "$autologinstr" ] || [ "$autologinstr" == "y" ]; then
    autologin="true"
fi
echo "Automatic login: $autologin"

for file in */*.ovpn; do
    chmod 0644 "$file"

    if [ "$autologin" == "true" ]; then
        echo "auth-user-pass $output/passwd" >> "$file"
    fi
done

if [ "$autologin" == "false" ]; then
    exit
fi

if [ -z "$username" ] || [ -z "$password" ]; then
    echo -n "Username: "
    read -r -e username
    echo -n "Password: "
    read -r -s password
    echo
fi

printf "%s\n%s" "$username" "$password" > passwd
chmod 0600 passwd

popd > /dev/null
