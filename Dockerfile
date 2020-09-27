FROM nixos/nix as build

WORKDIR /app

COPY src /app/src/
COPY *.cabal /app/
COPY *.nix /app/

RUN nix-build release.nix

ENTRYPOINT [ "/app/result/bin/dubloons" ]