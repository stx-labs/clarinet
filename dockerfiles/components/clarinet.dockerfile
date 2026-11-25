FROM debian:bookworm-slim

RUN apt update && apt install -y libssl-dev

RUN rustup update stable && rustup default stable

COPY clarinet /bin/

WORKDIR /workspace

ENTRYPOINT ["clarinet"]
