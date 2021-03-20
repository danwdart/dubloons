{ nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {},
  compiler ? "ghc8104" }: # basement doesn't yet like 901
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      dubloons = self.callCabal2nix "dubloons" (gitignore ./.) {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.dubloons
    ];
    buildInputs = [
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.wget
      nixpkgs.haskellPackages.stack
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.stylish-haskell
      nixpkgs.haskellPackages.hlint
    ];
    withHoogle = true;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.dubloons);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  dubloons = myHaskellPackages.dubloons;
}