# Nuri Language Compiler

[![Build Status](https://travis-ci.org/nurilabs/nuri-lang.svg?branch=master)](https://travis-ci.org/nurilabs/nuri-lang)
[![Gem Version](https://badge.fury.io/rb/nuric.png)](https://badge.fury.io/rb/nuric)


## Table of Contents

1. [Introduction](#intro)
2. [To Build](#build)
3. [Usage](#usage)
4. [Nuri Language](#nuri-language)
5. [Planning](#planning)
6. [License](#license)


<a name="intro"></a>
## Introduction

The Nuri language serves two objectives:

1. To specify a declarative specification of configuration state of a system.
2. To model a reconfiguration task which consists of:
    - an initial state
    - a goal state
    - a set of actions
    - global constraints

This compiler can do:
- syntax checking
- type checking
- evaluating the global constraints over the specification
- generating a compilation result in JSON
- automatically generate a plan of a reconfiguration (planning) problem -- this requires additional softwares

This compiler is used by the [Nuri](https://github.com/nurilabs/nuri) configuration management tool.


<a name="build"></a>
## To Build

Requirements:
- OCaml version >= 3.12.1
- m4 (required by ocamlfind)
- OPAM version >= 1.0.0
- ocamlfind (install via opam)
- Yojson version >= 1.1.8 (install via opam)

Compilation:
```bash
sudo apt-get install ocaml m4 opam
opam init
opam install ocamlfind yojson
cd ocaml
make
```

The above commands generates file `nuric`. Notes that in default the codes are compiled into OCaml byte-codes. However, we can compile it into native by setting variable `NATIVE=1`:
```bash
make NATIVE=1
```


<a name="usage"></a>
## Usage

The simplest way is:
```bash
nuric spec.nuri
```

This will parse the specification in file `spec.nuri`, perform the type-checking, evaluate the global constraints, and finally generate a compilation result in JSON.

Other options:
- `-x` : perform only syntax-checking
- `-t` : perform syntax and type-checking
- `-m` : return `root` instead of object `main`
- `-i` : specification from standard input (STDIN)
- `-g` : do not evaluate the global constraints
- `-p` : solve a reconfiguration (planning) problem

TODO: explain every option.


<a name="nuri-language"></a>
## Nuri Language

TODO


<a name="planning"></a>
## Planning

TODO


<a name="license"></a>
## License

[Apache License 2.0](https://github.com/nurilabs/nuri-lang/blob/master/LICENSE).
