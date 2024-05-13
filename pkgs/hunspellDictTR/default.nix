/*
hunspell dictionaries
*/
{
  lib,
  stdenv,
  fetchurl,
  fetchzip,
  fetchFromGitHub,
  unzip,
  coreutils,
  bash,
  which,
  zip,
  ispell,
  perl,
  python3,
  hunspell,
}: let
  mkDict = {
    pname,
    readmeFile,
    dictFileName,
    ...
  } @ args:
    stdenv.mkDerivation ({
        inherit pname;
        installPhase = ''
          runHook preInstall
          # hunspell dicts
          install -dm755 "$out/share/hunspell"
          install -m644 ${dictFileName}.dic "$out/share/hunspell/"
          install -m644 ${dictFileName}.aff "$out/share/hunspell/"
          # myspell dicts symlinks
          install -dm755 "$out/share/myspell/dicts"
          ln -sv "$out/share/hunspell/${dictFileName}.dic" "$out/share/myspell/dicts/"
          ln -sv "$out/share/hunspell/${dictFileName}.aff" "$out/share/myspell/dicts/"
          # docs
          install -dm755 "$out/share/doc"
          install -m644 ${readmeFile} $out/share/doc/${pname}.txt
          runHook postInstall
        '';
      }
      // args);
in rec {
  /*
  Turkish
  */
  tr_TR = tr_tr;
  tr_tr = mkDict rec {
    pname = "hunspell-dict-tr-tr";
    version = "1.1.1";

    src = fetchFromGitHub {
      owner = "tdd-ai";
      repo = "hunspell-tr";
      rev = version;
      sha256 = "0jma8mmrncyzd77kxliyngs4z6z4769g3nh0a7xn2pd4s5y2xdpy";
    };

    dictFileName = "tr_TR";
    readmeFile = "README.md";

    meta = with lib; {
      description = "Hunspell dictionary for Turkish from tdd-ai";
      homepage = "https://github.com/tdd-ai/hunspell-tr/";
      license = with licenses; [mpl20];
    };
  };
}
