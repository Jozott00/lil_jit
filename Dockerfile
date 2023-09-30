FROM --platform=linux/arm64 arm64v8/rust
LABEL authors="jozott"

RUN apt-get update &&  \
    apt-get install -y libclang-dev

WORKDIR /workspace

COPY Cargo.toml Cargo.lock /workspace/
RUN cargo fetch

COPY . /workspace/
