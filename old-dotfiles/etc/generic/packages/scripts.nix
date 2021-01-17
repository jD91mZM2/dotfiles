{ config, lib, pkgs, ... }:

let
  script = name: message: arg:
    let
      usage = "${name} <remote> <ports...>";
    in
    pkgs.writeShellScriptBin name ''
      remote="''${1:?${usage}}"
      : "''${2:?${usage}}"
      cat <<-EOF
      ${message}

      If it doesn't seem to work, make sure the remote's sshd_config
      specifies "GatewayPorts" to either "yes" or "clientspecified".
      EOF

      # Shift away first argument
      shift

      arguments=()
      for port in "$@"; do
        arguments+=(${arg} ":''${port}:localhost:''${port}")
      done

      set -x
      ssh "$remote" "''${arguments[@]}" -- sleep infinity
    '';
  forward = script "forward" "Remote port being forwarded over SSH!" "-R";
  backward = script "backward" "Local port being forwarded to a remote application over SSH!" "-L";
in
{
  setup.home.modules = lib.singleton {
    home.packages = [
      forward
      backward
    ];
  };
}
