{
  networking.firewall = {
    enable = true;
    # Important: iptables applies whatever rule matches first.
    # So keep the catch-all rule LAST.
    extraCommands = ''
      # Delete old rules
      iptables -F INPUT
      iptables -F FORWARD
      iptables -F OUTPUT

      # Allow DNS servers
      iptables -A INPUT -s 1.1.1.1 -j ACCEPT
      iptables -A OUTPUT -d 1.1.1.1 -j ACCEPT
      iptables -A INPUT -s 1.0.0.1 -j ACCEPT
      iptables -A OUTPUT -d 1.0.0.1 -j ACCEPT

      # Allow new connections to NordVPN, and all other UDP connections over port 1194
      iptables -A INPUT -p udp --sport 1194 -j ACCEPT
      iptables -A OUTPUT -p udp --dport 1194 -j ACCEPT

      # Allow connections over VPN interface
      iptables -A INPUT -i tun+ -j ACCEPT
      iptables -A OUTPUT -o tun+ -j ACCEPT

      # Allow local connections/local ips
      iptables -A INPUT -s 10.0.0.0/8 -j ACCEPT
      iptables -A OUTPUT -d 10.0.0.0/8 -j ACCEPT
      iptables -A INPUT -s 172.16.0.0/12 -j ACCEPT
      iptables -A OUTPUT -d 172.16.0.0/12 -j ACCEPT
      iptables -A INPUT -s 192.168.0.0/16 -j ACCEPT
      iptables -A OUTPUT -d 192.168.0.0/16 -j ACCEPT

      # Allow localhost
      iptables -A INPUT -i lo -j ACCEPT
      iptables -A OUTPUT -o lo -j ACCEPT

      # Drop everything else!
      iptables -A INPUT -j DROP
      iptables -A OUTPUT -j DROP
    '';
  };
}
