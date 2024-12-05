{pkgs}:
pkgs.stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "1.009";

  src = ./.;

  installPhase = ''
    mkdir -p $out/share/fonts/truetype/
    if [ -d "$src/bm-font" ]; then
      cp -r $src/bm-font/*.ttf $out/share/fonts/truetype/
    else
      echo "No fonts found in $src/bm-font"
      exit 0
    fi
  '';
}
