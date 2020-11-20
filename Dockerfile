FROM fpco/stack-build:latest
WORKDIR /app
COPY . .
RUN stack build
CMD ["stack", "run"]