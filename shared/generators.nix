# Things that should be imported using `imports = [ ... ]`

# Cannot be put in utils.nix, as that depends on nixpkgs and causes an
# infinite recursion.
{
  serviceUser = { name, script }: {
    users.users."${name}" = {
      createHome = true;
      home = "/var/lib/${name}";
    };
    systemd.services."${name}" = {
      inherit script;
      serviceConfig = {
        User = name;
        WorkingDirectory = "/var/lib/${name}";
        Restart = "always";
        RestartSec = "10s";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
