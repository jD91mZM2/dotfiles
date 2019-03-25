{ rustPlatform, openssl, pkgconfig, sqlite }:

rustPlatform.buildRustPackage {
  name = "mcbotface";

  src = ~/Coding/Rust/mcbotface;
  cargoSha256 = "1m7h4f3xvlg0wzmkbc5hv31d4pslw3nkp47gh2i10b5iv9mx7rp8";

  buildInputs = [
    openssl
    pkgconfig
    sqlite
  ];
}
