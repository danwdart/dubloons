FROM nixos/nix
COPY . .
RUN nix-env -i git
RUN nix-build
CMD ["dubloons"]
