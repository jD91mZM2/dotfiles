{ lib, pkgs, ... }:

let
  ssh-scan = pkgs.writers.writeBashBin "ssh-scan" ''
    if [ "$EUID" -ne 0 ]; then
      echo "Results are sometimes inconsistent without root, run:"
      echo "$ sudo ssh-scan"
      exit 1
    fi
    ${pkgs.nmap}/bin/nmap -p22 192.168.0.0/24 --open -T5 -sV
  '';
in
{
  security.sudo.extraRules = lib.mkAfter [{
    groups = ["wheel"];
    commands = [
      { command = ''${ssh-scan}/bin/ssh-scan ""''; options = [ "SETENV" "NOPASSWD" ]; }
    ];
  }];

  environment.systemPackages = [
    ssh-scan
  ];
}
