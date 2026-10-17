# Trixie is the codename for Debian 13; slim is its reduced-size container image.
FROM debian:trixie-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends build-essential ca-certificates curl git \
    && rm -rf /var/lib/apt/lists/*

ENV PATH="/root/.cargo/bin:${PATH}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o /tmp/rustup-init.sh \
    && sh /tmp/rustup-init.sh -y --profile minimal --default-toolchain none \
    && rm /tmp/rustup-init.sh

WORKDIR /elaborate
COPY rust-toolchain.toml .
RUN rustup install

COPY . .
CMD ["sh", "-c", "BLESS=1 cargo test --locked -p generate --lib test::std_json -- --exact"]
