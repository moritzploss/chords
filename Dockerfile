FROM haskell:8.8.3 AS build
WORKDIR /usr/src/app

RUN apt update
RUN apt install libpcre3-dev -y

COPY package.yaml package.yaml
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY Setup.hs Setup.hs

RUN stack setup

COPY app/ app/
COPY test/ test/
COPY src/ src/
COPY README.md README.md
COPY ChangeLog.md ChangeLog.md

RUN stack build --copy-bins

FROM debian

COPY --from=build /root/.local/bin/chords-exe /usr/local/bin/chords-exe

EXPOSE 8080
CMD ["chords-exe"]