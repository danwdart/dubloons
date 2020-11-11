FROM arm64v8/ubuntu
WORKDIR /app
COPY . .
RUN apt update && apt -y install cabal-install ghc && apt clean
RUN cabal update
RUN cabal new-install
ENTRYPOINT [ "dubloons" ]