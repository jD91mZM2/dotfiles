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

  # Add ad-hoc rustup binaries to $PATH
  home.home.sessionPath = [ "$HOME/.cargo/bin" ];
}
