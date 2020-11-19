FROM fpco/stack-build as builder

COPY . .

RUN stack install

FROM ubuntu

COPY --from=builder /root/.local/bin/dubloons /usr/bin/

CMD ["dubloons"]
