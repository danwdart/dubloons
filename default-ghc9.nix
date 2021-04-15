{ nixpkgs ? import <unstable> {},
  compiler ? "ghc901" }: # basement doesn't yet like 901
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # TODO fix cryptonite to use integer-gmp 1.1
      # https://github.com/haskell-crypto/cryptonite/issues/340
      cryptonite = self.callHackage "cryptonite" "0.28" {};
      # Earliest version supporting ghc 9.0.1
      hxt = self.callHackage "hxt" "9.3.1.22" {};
      # https://github.com/vincenthz/hs-memory/issues/84
      memory = (self.callHackage "memory" "0.15.0" {}).overrideDerivation(self: {
        prePatch = ''
          sed -zi 's/import\(\s\+\)GHC.Prim/import\1GHC.Prim hiding (unsafeCoerce#)\nimport\1GHC.Exts (unsafeCoerce#)/' Data/ByteArray/Bytes.hs
          sed -zi 's/import\(\s\+\)GHC.Prim/import\1GHC.Prim hiding (unsafeCoerce#)\nimport\1GHC.Exts (unsafeCoerce#)/' Data/ByteArray/ScrubbedBytes.hs
        '';
      });
      # https://github.com/Soostone/retry/issues/71
      retry = (self.callHackage "retry" "0.8.1.2" {}).overrideDerivation(self: {
        prePatch = ''
          sed -i 's/fmap \(.*\) genDuration/fmap (\\x -> \1 x) genDuration/g' test/Tests/Control/Retry.hs
        '';
      });
      profunctors = self.callHackage "profunctors" "5.6.2" {};
      # https://github.com/haskell-foundation/foundation/issues/551
      foundation = (self.callHackage "foundation" "0.0.25" {}).overrideDerivation(self: {
        prePatch = ''
          sed -i 's/return . flip unConduit rest . onExc/\\x -> return \$ unConduit (onExc x) rest/g' Foundation/Conduit/Internal.hs
        '';
      });
      # https://github.com/haskell-foundation/foundation/issues/548 ->
      # https://github.com/haskell-foundation/foundation/pull/549
      basement = (self.callCabal2nixWithOptions "basement" (builtins.fetchGit {
        url = "https://github.com/LaurentRDC/foundation.git";
        rev = "d02ab182c977272ae4089ce20d15d82fc4fcd5e8";
      }) "--subpath basement" {});
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
    # withHoogle = true;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.dubloons);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  dubloons = myHaskellPackages.dubloons;
}