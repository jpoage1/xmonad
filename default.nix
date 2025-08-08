{ pkgs ? import <nixpkgs> { } }:

let
  myGhc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    random-shuffle
    xmonad
    xmonad-contrib
    process
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-config";
  src = pkgs.fetchGit {
    url = "file://${toString ./}";
    ref = "refs/heads/main";
  };
  buildInputs = [
    myGhc
  ];

  buildPhase = ''
    ${myGhc}/bin/ghc --make xmonad.hs -isrc -o xmonad
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp xmonad $out/bin/
  '';
}
