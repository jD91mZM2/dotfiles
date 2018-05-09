{
  services.openvpn.servers = {
    sweden = {
      config = "config /root/openvpn/Sweden.conf";
      autoStart = true;
    };

    au-melbourne = { config = "config /root/openvpn/AU_Melbourne.conf"; autoStart = false; };
    austria = { config = "config /root/openvpn/Austria.conf"; autoStart = false; };
    au-sydney = { config = "config /root/openvpn/AU_Sydney.conf"; autoStart = false; };
    belgium = { config = "config /root/openvpn/Belgium.conf"; autoStart = false; };
    brazil = { config = "config /root/openvpn/Brazil.conf"; autoStart = false; };
    ca-montreal = { config = "config /root/openvpn/CA_Montreal.conf"; autoStart = false; };
    ca-toronto = { config = "config /root/openvpn/CA_Toronto.conf"; autoStart = false; };
    ca-vancouver = { config = "config /root/openvpn/CA_Vancouver.conf"; autoStart = false; };
    czech-republic = { config = "config /root/openvpn/Czech_Republic.conf"; autoStart = false; };
    denmark = { config = "config /root/openvpn/Denmark.conf"; autoStart = false; };
    finland = { config = "config /root/openvpn/Finland.conf"; autoStart = false; };
    france = { config = "config /root/openvpn/France.conf"; autoStart = false; };
    germany = { config = "config /root/openvpn/Germany.conf"; autoStart = false; };
    hong-kong = { config = "config /root/openvpn/Hong_Kong.conf"; autoStart = false; };
    india = { config = "config /root/openvpn/India.conf"; autoStart = false; };
    ireland = { config = "config /root/openvpn/Ireland.conf"; autoStart = false; };
    israel = { config = "config /root/openvpn/Israel.conf"; autoStart = false; };
    italy = { config = "config /root/openvpn/Italy.conf"; autoStart = false; };
    japan = { config = "config /root/openvpn/Japan.conf"; autoStart = false; };
    mexico = { config = "config /root/openvpn/Mexico.conf"; autoStart = false; };
    netherlands = { config = "config /root/openvpn/Netherlands.conf"; autoStart = false; };
    new-zealand = { config = "config /root/openvpn/New_Zealand.conf"; autoStart = false; };
    norway = { config = "config /root/openvpn/Norway.conf"; autoStart = false; };
    romania = { config = "config /root/openvpn/Romania.conf"; autoStart = false; };
    singapore = { config = "config /root/openvpn/Singapore.conf"; autoStart = false; };
    spain = { config = "config /root/openvpn/Spain.conf"; autoStart = false; };
    switzerland = { config = "config /root/openvpn/Switzerland.conf"; autoStart = false; };
    turkey = { config = "config /root/openvpn/Turkey.conf"; autoStart = false; };
    uk-london = { config = "config /root/openvpn/UK_London.conf"; autoStart = false; };
    uk-manchester = { config = "config /root/openvpn/UK_Manchester.conf"; autoStart = false; };
    uk-southampton = { config = "config /root/openvpn/UK_Southampton.conf"; autoStart = false; };
    us-atlanta = { config = "config /root/openvpn/US_Atlanta.conf"; autoStart = false; };
    us-california = { config = "config /root/openvpn/US_California.conf"; autoStart = false; };
    us-chicago = { config = "config /root/openvpn/US_Chicago.conf"; autoStart = false; };
    us-east = { config = "config /root/openvpn/US_East.conf"; autoStart = false; };
    us-florida = { config = "config /root/openvpn/US_Florida.conf"; autoStart = false; };
    us-houston = { config = "config /root/openvpn/US_Houston.conf"; autoStart = false; };
    us-las-vegas = { config = "config /root/openvpn/US_Las_Vegas.conf"; autoStart = false; };
    us-midwest = { config = "config /root/openvpn/US_Midwest.conf"; autoStart = false; };
    us-new-york-city = { config = "config /root/openvpn/US_New_York_City.conf"; autoStart = false; };
    us-seattle = { config = "config /root/openvpn/US_Seattle.conf"; autoStart = false; };
    us-silicon-valley = { config = "config /root/openvpn/US_Silicon_Valley.conf"; autoStart = false; };
    us-texas = { config = "config /root/openvpn/US_Texas.conf"; autoStart = false; };
    us-west = { config = "config /root/openvpn/US_West.conf"; autoStart = false; };
  };
}
