FROM --platform=linux/arm64 arm64v8/rust:latest AS builder
LABEL authors="jozott"

RUN apt-get update  \
    && apt-get install -y libclang-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /usr/src/app

# Copy the source code and build the project
COPY . .
RUN cargo build --release

# Second stage: Create the final image
FROM ubuntu:latest

# Copy the compiled executable from the first stage
COPY --from=builder /usr/src/app/target/release/lil_jit /usr/local/bin/

WORKDIR /app
# Set the entry point to your compiled JIT compiler
ENTRYPOINT ["/usr/local/bin/lil_jit"]