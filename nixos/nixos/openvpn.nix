{
  services.openvpn.servers = {
    sweden = {
      config = "config /root/openvpn/Sweden.conf";
      autoStart = true;
    };

    au-melbourne = { config = "config /root/openvpn/AU-Melbourne.conf"; autoStart = false; };
    austria = { config = "config /root/openvpn/Austria.conf"; autoStart = false; };
    au-sydney = { config = "config /root/openvpn/AU-Sydney.conf"; autoStart = false; };
    belgium = { config = "config /root/openvpn/Belgium.conf"; autoStart = false; };
    brazil = { config = "config /root/openvpn/Brazil.conf"; autoStart = false; };
    ca-montreal = { config = "config /root/openvpn/CA-Montreal.conf"; autoStart = false; };
    ca-toronto = { config = "config /root/openvpn/CA-Toronto.conf"; autoStart = false; };
    ca-vancouver = { config = "config /root/openvpn/CA-Vancouver.conf"; autoStart = false; };
    czech-republic = { config = "config /root/openvpn/Czech-Republic.conf"; autoStart = false; };
    denmark = { config = "config /root/openvpn/Denmark.conf"; autoStart = false; };
    finland = { config = "config /root/openvpn/Finland.conf"; autoStart = false; };
    france = { config = "config /root/openvpn/France.conf"; autoStart = false; };
    germany = { config = "config /root/openvpn/Germany.conf"; autoStart = false; };
    hong-kong = { config = "config /root/openvpn/Hong-Kong.conf"; autoStart = false; };
    india = { config = "config /root/openvpn/India.conf"; autoStart = false; };
    ireland = { config = "config /root/openvpn/Ireland.conf"; autoStart = false; };
    israel = { config = "config /root/openvpn/Israel.conf"; autoStart = false; };
    italy = { config = "config /root/openvpn/Italy.conf"; autoStart = false; };
    japan = { config = "config /root/openvpn/Japan.conf"; autoStart = false; };
    mexico = { config = "config /root/openvpn/Mexico.conf"; autoStart = false; };
    netherlands = { config = "config /root/openvpn/Netherlands.conf"; autoStart = false; };
    new-zealand = { config = "config /root/openvpn/New-Zealand.conf"; autoStart = false; };
    norway = { config = "config /root/openvpn/Norway.conf"; autoStart = false; };
    romania = { config = "config /root/openvpn/Romania.conf"; autoStart = false; };
    singapore = { config = "config /root/openvpn/Singapore.conf"; autoStart = false; };
    spain = { config = "config /root/openvpn/Spain.conf"; autoStart = false; };
    switzerland = { config = "config /root/openvpn/Switzerland.conf"; autoStart = false; };
    turkey = { config = "config /root/openvpn/Turkey.conf"; autoStart = false; };
    uk-london = { config = "config /root/openvpn/UK-London.conf"; autoStart = false; };
    uk-manchester = { config = "config /root/openvpn/UK-Manchester.conf"; autoStart = false; };
    uk-southampton = { config = "config /root/openvpn/UK-Southampton.conf"; autoStart = false; };
    us-atlanta = { config = "config /root/openvpn/US-Atlanta.conf"; autoStart = false; };
    us-california = { config = "config /root/openvpn/US-California.conf"; autoStart = false; };
    us-chicago = { config = "config /root/openvpn/US-Chicago.conf"; autoStart = false; };
    us-east = { config = "config /root/openvpn/US-East.conf"; autoStart = false; };
    us-florida = { config = "config /root/openvpn/US-Florida.conf"; autoStart = false; };
    us-houston = { config = "config /root/openvpn/US-Houston.conf"; autoStart = false; };
    us-las-vegas = { config = "config /root/openvpn/US-Las-Vegas.conf"; autoStart = false; };
    us-midwest = { config = "config /root/openvpn/US-Midwest.conf"; autoStart = false; };
    us-new-york-city = { config = "config /root/openvpn/US-New-York-City.conf"; autoStart = false; };
    us-seattle = { config = "config /root/openvpn/US-Seattle.conf"; autoStart = false; };
    us-silicon-valley = { config = "config /root/openvpn/US-Silicon-Valley.conf"; autoStart = false; };
    us-texas = { config = "config /root/openvpn/US-Texas.conf"; autoStart = false; };
    us-west = { config = "config /root/openvpn/US-West.conf"; autoStart = false; };
  };
}
