# contriver

[![Build Status](https://travis-ci.org/henrytill/contriver.svg?branch=master)](https://travis-ci.org/henrytill/contriver)

`contriver` is a small Lisp-1 interpreter.

## Requirements

To build and use `contriver`, you will need:

* [OCaml](https://ocaml.org/) (version 4.02 or greater)
* [opam](https://opam.ocaml.org/)
* [Make](https://www.gnu.org/software/make/)

## Usage

```sh
$ make deps
  ...
$ make
  ...
$ make test
  ...
$ ./main.native -i
Welcome to contriver
><> (define add1 (lambda (x) (+ x 1)))
(lambda (x) ...)
><> (add1 41)
42
><>
```

## Running contriver inside Emacs

```elisp
(run-scheme "<path/to/contriver/executable>/main.native -i")
```

or

```elisp
(setq scheme-program-name "<path/to/contriver/executable>/main.native -i")
(run-scheme)
```
