FROM rust:1.76-slim AS builder

WORKDIR /usr/src/moshell

RUN apt-get update \
    && apt-get -y --no-install-recommends install cmake build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN cargo build --release --bin moshell && strip target/release/moshell

FROM alpine:3.18 AS runtime

RUN apk add --no-cache libgcc libstdc++ gcompat
COPY --from=builder /usr/src/moshell/target/release/moshell /bin/moshell
COPY lib /usr/share/moshell/lib

ENTRYPOINT ["/bin/moshell"]
