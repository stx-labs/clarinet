# This image is build in Clarinet CI with the `clarinet` artifact built in the `dist_clarinet` job

FROM debian:bookworm-slim

RUN apt update && apt install -y libssl-dev

COPY clarinet /bin/

WORKDIR /workspace

ENTRYPOINT ["clarinet"]
