![test](https://github.com/moritzploss/chords/workflows/Tests/badge.svg)

# Chords

A small tool to look up and transpose piano chords, built with Haskell.

## Setup

Set-up a Haskell development environment. On macOS:

    brew install haskell-stack

To generate a `.cabal` file and build the backend, run:

    stack build

Then run the tests:

    stack test

## Run with Docker

Build the Docker image:

    docker build -t chords .

Then start a container:

    docker run -p 8080:8080 chords:latest

## Start the Backend

To run the backend on `localhost:8080`:

    stack build --copy-bins
    chords-exe

Then send a `post` request to the `localhost:8080/api/v1/chords` endpoint,
including a JSON body that contains a chord name (string) and the interval by
which to transpose the chord (integer, optional):

```javascript
{
    "chord": "Dmin",
    "transpose": 4
}
```
