# The Galileo REPL in a small image:
#   docker run -it --rm cascala/galileo
#   docker run --rm -v "$PWD:/work" cascala/galileo script.gg
#   docker run -it --rm -v "$PWD:/work" cascala/galileo -i script.gg

FROM rust:1-slim AS build
WORKDIR /src
COPY . .
RUN cargo build --release --locked --bin galileo

FROM debian:trixie-slim
COPY --from=build /src/target/release/galileo /usr/local/bin/galileo
RUN useradd -m -u 1000 galileo && mkdir /work && chown galileo:galileo /work
USER galileo
WORKDIR /work
ENTRYPOINT [ "galileo" ]
