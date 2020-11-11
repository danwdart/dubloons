FROM haskell AS build
WORKDIR /app
COPY . .
RUN stack install
ENTRYPOINT [ "dubloons" ]