# build the executable
FROM haskell:8.8.3 AS build
WORKDIR /usr/src/app

# required by pcre-light
RUN apt update
RUN apt install libpcre3-dev -y

COPY package.yaml package.yaml
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY Setup.hs Setup.hs

RUN stack setup
RUN stack build --only-dependencies

COPY app/ app/
COPY test/ test/
COPY src/ src/
COPY README.md README.md
COPY ChangeLog.md ChangeLog.md

RUN stack build --copy-bins

# copy executable to fresh image
FROM debian

COPY --from=build /root/.local/bin/chords-exe /usr/local/bin/chords-exe

EXPOSE 8080
CMD ["chords-exe"]