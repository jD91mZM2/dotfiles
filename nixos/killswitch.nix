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
      iptables -A INPUT -d 1.1.1.1 -j ACCEPT
      iptables -A OUTPUT -d 1.1.1.1 -j ACCEPT
      iptables -A INPUT -d 1.0.0.1 -j ACCEPT
      iptables -A OUTPUT -d 1.0.0.1 -j ACCEPT

      # Allow new connections to Private Internet Access, port 1197
      iptables -A INPUT -p udp --dport 1197 -j ACCEPT
      iptables -A OUTPUT -p udp --dport 1197 -j ACCEPT

      # Allow connections over VPN interface
      iptables -A INPUT -i tun+ -j ACCEPT
      iptables -A OUTPUT -o tun+ -j ACCEPT

      # Allow local connections
      iptables -A INPUT -d 192.168.2.0/24 -j ACCEPT
      iptables -A OUTPUT -d 192.168.2.0/24 -j ACCEPT

      # Drop everything else!
      iptables -A INPUT -j DROP
      iptables -A OUTPUT -j DROP
    '';
  };
}
