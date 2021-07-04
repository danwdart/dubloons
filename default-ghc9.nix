{ nixpkgs ? import <unstable> {},
  compiler ? "ghc901" }: # basement doesn't yet like 901
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      cryptonite = self.callCabal2nix "cryptonite" (nixpkgs.pkgs.fetchFromGitHub {
        owner = "haskell-crypto";
        repo = "cryptonite";
        rev = "cryptonite-v0.29";
        sha256 = "12nvrsrb1pdkif8nykiaby4sksry3kxq1j89z6x67h66232h5ril";
      }) {};
      # hxt = self.callHackage "hxt" "9.3.1.22" {};
      memory = self.callCabal2nix "memory" (nixpkgs.pkgs.fetchFromGitHub {
        owner = "vincenthz";
        repo = "hs-memory";
        rev = "memory-v0.16.0";
        sha256 = "12nvrsrb1pdkif8nykiaby4sksry3kxq1j89z6x67h66232h5ril";
      }) {};
      # retry = self.callHackage "retry" "0.9.0.0" {};
      profunctors = self.callHackage "profunctors" "5.6.2" {};
      foundation = self.callCabal2nixWithOptions "foundation" (nixpkgs.pkgs.fetchFromGitHub {
        owner = "haskell-foundation";
        repo = "foundation";
        rev = "foundation-v0.0.26.1";
        sha256 = "0a61c9b6zmcqy2phrz77ndjywsycxp3gz2k9m2vrzyg0gqj1a078";
      }) "--subpath foundation" {};
      basement = self.callCabal2nixWithOptions "basement" (nixpkgs.pkgs.fetchFromGitHub {
        owner = "haskell-foundation";
        repo = "foundation";
        rev = "foundation-v0.0.26.1";
        sha256 = "0a61c9b6zmcqy2phrz77ndjywsycxp3gz2k9m2vrzyg0gqj1a078";
      }) "--subpath basement" {};
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