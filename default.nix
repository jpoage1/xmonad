{ pkgs ? import <nixpkgs> { } }:

let
  myGhc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    random-shuffle
    xmonad
    xmonad-contrib
    process
    monad-logger
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-config";
  src = ./.;
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
