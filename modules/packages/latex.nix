{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Install texlive
    (texlive.combine {
      inherit (texlive)
        # Main suite
        scheme-medium
        # Extra packages
        numprint
        tkz-base
        tkz-euclide
        ;
    })

    # Language Server
    texlab

    # Install PDF viewer
    zathura
  ];
}
