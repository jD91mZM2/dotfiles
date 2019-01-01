#!/usr/bin/env bash

set -e

# https://stackoverflow.com/a/29613573
quoteSubst() {
  IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$1")
  printf %s "${REPLY%$'\n'}"
}

echo -n "Output directory [./openvpn]: "
read -r output
if [ -z "$output" ]; then
    output="openvpn"
fi
output="$(realpath "$output")"

if [ -e "$output" ]; then
    echo "Output directory already exists"
    exit
fi

file="$(mktemp)"
wget "https://www.privateinternetaccess.com/openvpn/openvpn-strong.zip" -O "$file"
unzip -q "$file" -d "$output"

cd "$output"

echo -n "Configure with automatic login? [Y/n] "
read -r autologinstr
autologin="false"
if [ -z "$autologinstr" ] || [ "$autologinstr" == "y" ]; then
    autologin="true"
fi
echo "Automatic login: $autologin"

for old_file in *.ovpn; do
    file="$(echo -n "$old_file" | sed -e 's/ /_/g' -e 's/\.ovpn$/\.conf/')"
    mv "$old_file" "$file"

    sed 's/\(crl-verify \)\(crl.rsa.4096.pem\)/\1'"$(quoteSubst "$output")"'\/\2/' -i "$file"
    sed 's/\(ca \)\(ca.rsa.4096.crt\)/\1'"$(quoteSubst "$output")"'\/\2/' -i "$file"

    if [ "$autologin" == "true" ]; then
        echo "auth-user-pass $output/passwd" >> "$file"
    fi
done

if [ "$autologin" == "false" ]; then
    exit
fi

echo -n "PIA Username: "
read -r username
echo -n "PIA Password: "
read -r password

printf "%s\n%s" "$username" "$password" > passwd
chmod 0600 passwd
