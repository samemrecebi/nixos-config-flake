# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {
  # example = pkgs.callPackage ./example { };
  hunspellDictTR = pkgs.callPackage ./hunspellDictTR {};
}
