{ pkgs, ... }:
{
  environment = {
    # Install rustup
    systemPackages = with pkgs; [
      rustup
    ];

    # Enable backtrace
    variables.RUST_BACKTRACE = "1";
  };

  home = {
    # Add ad-hoc rustup binaries to $PATH
    home.sessionPath = [ "$HOME/.cargo/bin" ];

    # Rustfmt config
    xdg.configFile."rustfmt/rustfmt.toml".text = ''
      unstable_features = true

      # Merge imports, please
      imports_granularity = "Crate" # new way
      merge_imports = true          # old way
    '';
  };
}
