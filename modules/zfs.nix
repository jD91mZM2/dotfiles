{
  # Support for zfs
  boot.supportedFilesystems = [ "zfs" ];

  # Backup
  services.zfs = {
    autoSnapshot = {
      enable = true;

      # I never use these anyway, as everything I throw away goes in a
      # trashcan.
      frequent = 2;
      hourly = 5;
      daily = 3;
      weekly = 2;
      monthly = 0;
    };

    # Scrub every once in a while
    autoScrub.enable = true;

    # Run an SSD TRIM every once in a while
    trim.enable = true;
  };
}
