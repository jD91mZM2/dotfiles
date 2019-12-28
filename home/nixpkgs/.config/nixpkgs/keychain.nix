{ config, pkgs, lib, ... }:

let
  askpass = (pkgs.callPackage ~/Coding/Scripting/ssh-askpass {}).override {
    globalGrab = false;
  };
  ssh = pkgs.writeShellScriptBin "ssh" ''
    ${pkgs.keychain}/bin/keychain --quiet ~/.ssh/id_ed25519 || true
    ${pkgs.openssh}/bin/ssh "$@"
  '';

  keychainCfg = config.programs.keychain;

  keychainFlags = keychainCfg.extraFlags
                  ++ lib.optional (keychainCfg.agents != []) "--agents ${lib.concatStringsSep "," keychainCfg.agents}"
                  ++ lib.optional (keychainCfg.inheritType != null) "--inherit ${keychainCfg.inheritType}";

  keychainShellCommand = "${keychainCfg.package}/bin/keychain --eval ${lib.concatStringsSep " " keychainFlags} ${lib.concatStringsSep " " keychainCfg.keys}";
in
{
  # Main services
  programs.keychain = {
    enable                = true;
    enableBashIntegration = true;
    enableZshIntegration  = true;
    agents                = [ "ssh" "gpg" ];
    extraFlags            = [ "--noask" "--quiet" ];
    keys                  = [];
  };
  services.gpg-agent = {
    enable             = true;
    defaultCacheTtl    = 86400;
    maxCacheTtl        = 86400;
  };

  # Override askpass
  home.sessionVariables.SSH_ASKPASS = "${askpass}/bin/ssh-askpass.py";

  xsession.initExtra = ''
    eval "$(${keychainShellCommand})"
  '';

  # Dynamically add key to keychain upon first launch
  home.packages = [ ssh ];
  programs.git.extraConfig.core.sshCommand = "${ssh}/bin/ssh";
}
