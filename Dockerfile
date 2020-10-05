FROM haskell
WORKDIR /usr/src/app

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

EXPOSE 8080
CMD [ "chords-exe"]