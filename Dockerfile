# The Galileo REPL in a small image:
#   docker run -it --rm cascala/galileo
#   docker run --rm -v "$PWD:/work" cascala/galileo script.gg
#   docker run -it --rm -v "$PWD:/work" cascala/galileo -i script.gg
#   docker run -i --rm cascala/galileo mcp          (MCP server for AI assistants)

FROM rust:1-slim-bookworm AS build
WORKDIR /src
COPY . .
RUN cargo build --release --locked --bin galileo && mkdir -p /work /home/galileo

# Distroless: glibc and libgcc only, no shell or package manager; runs as user "nonroot".
FROM gcr.io/distroless/cc-debian12:nonroot
COPY --from=build /src/target/release/galileo /usr/local/bin/galileo
COPY --from=build --chown=nonroot:nonroot /work /work
COPY --from=build --chown=nonroot:nonroot /home/galileo /home/galileo
ENV HOME=/home/galileo
WORKDIR /work
ENTRYPOINT [ "galileo" ]
