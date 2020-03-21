# Political Quiz Prototype

## Requirements

[Install the Nix package manager](https://nixos.org/nix/download.html)

Also make sure that ~/.cabal/bin is in your path

## To install

```bash
$ git clone https://github.com/epeery/quiz.git

$ cd quiz

$ nix-shell

$ cabal install

$ cd results

$ bash setup.sh

$ npm install
```

## Useage

```bash
$ quiz-server --port PORT
```
This must be executed at the root of the project

