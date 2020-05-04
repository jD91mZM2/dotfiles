self: super:

{
  # yaru-dracula-theme = super.callPackage (builtins.fetchTarball https://github.com/jD91mZM2/yaru-dracula/archive/master.tar.gz) {};
  # ^ See https://github.com/ubuntu/yaru/issues/2079
  yaru-dracula-theme = super.yaru-theme;
}
