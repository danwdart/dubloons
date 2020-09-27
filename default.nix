{ mkDerivation, aeson, base, bytestring, containers
, discord-haskell, feed, http-client, hxt, hxt-xpath, mtl, process
, req, retry, safe-foldable, stdenv, text, transformers, unix
, xml-types
}:
mkDerivation {
  pname = "dubloons";
  version = "0.6.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers discord-haskell feed http-client
    hxt hxt-xpath mtl process req retry safe-foldable text transformers
    unix xml-types
  ];
  homepage = "https://github.com/danwdart/dubloons#readme";
  license = stdenv.lib.licenses.agpl3;
}
