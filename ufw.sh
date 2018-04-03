#!/bin/sh

sudo ufw reset

# VPN Internet Killswitch
## Deny by default
sudo ufw default deny outgoing
sudo ufw default deny incoming

## Allow OpenVPN's network adapter, tun0
sudo ufw allow out on tun0
sudo ufw allow in on tun0

## Allow local connections
sudo ufw allow out to 192.168.2.0/24
sudo ufw allow in from 192.168.2.0/24

## Private Internet Access VPN has A LOT of IPs.
## I tried getting 'em all using nslookup <endpoint>.privateinternetaccess.com,
## but it didn't cover all of them for some reason.
## So here is me being a lazy fuck:
sudo ufw allow in 1198/udp
sudo ufw allow out 1198/udp

## DNS Servers
sudo ufw allow in from 1.1.1.1
sudo ufw allow in from 1.0.0.1
sudo ufw allow out to 1.1.1.1
sudo ufw allow out to 1.0.0.1

sudo ufw enable
