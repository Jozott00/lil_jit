FROM --platform=linux/arm64 arm64v8/rust
LABEL authors="jozott"

RUN apt-get update &&  \
    apt-get install -y libclang-dev

WORKDIR /workspace
COPY . .

RUN cargo build --release

ENTRYPOINT ["target/release/lil_jit"]
CMD ["examples/mandelbrot.lil"]

