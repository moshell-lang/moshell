FROM rust:1.79-slim AS builder

WORKDIR /usr/src/moshell

RUN apt-get update \
    && apt-get -y --no-install-recommends install cmake build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN cargo build --release --bin moshell && strip target/release/moshell

FROM busybox:glibc AS runtime

COPY --from=builder "/usr/lib/x86_64-linux-gnu/libstdc++.so.6" "/lib64/libstdc++.so.6"
COPY --from=builder "/usr/lib/x86_64-linux-gnu/libgcc_s.so.1" "/lib64/libgcc_s.so.1"
COPY --from=builder /usr/src/moshell/target/release/moshell /bin/moshell
COPY lib /usr/share/moshell/lib

ENTRYPOINT ["/bin/moshell"]
