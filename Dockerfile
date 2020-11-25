FROM dandart/dubloons:build as builder
RUN stack install

FROM ubuntu
COPY --from=builder /root/.local/bin/dubloons /usr/bin/
CMD ["dubloons"]