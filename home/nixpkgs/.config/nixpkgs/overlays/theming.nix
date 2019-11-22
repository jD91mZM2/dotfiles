self: super:

{
  yaru-dracula-theme = super.callPackage (builtins.fetchTarball https://github.com/jD91mZM2/yaru-dracula/archive/master.tar.gz) {};
}
