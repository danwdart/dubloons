FROM fpco/stack-build as builder
COPY . .
RUN stack build