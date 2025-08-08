{ pkgs ? import <nixpkgs> {  } }:
let
  myGhc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    random-shuffle
    xmonad
    xmonad-contrib
    process
  ]);
  my-xmonad = pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. {};
in

pkgs.haskellPackages.shellFor {
  packages = p: [ p.xmonad my-xmonad ];

  withHoogle = true;

  # Add some development tools to the shell.
  nativeBuildInputs = (with pkgs; [
    cabal-install
    haskell-language-server
  ]) ++ (with pkgs.haskellPackages; [
    hlint
    ormolu
    weeder
    random-shuffle
    xmonad
    xmonad-contrib
    process
  ]);


  shellHook = pkgs.haskellPackages.xmonad.env.shellHook;

}
