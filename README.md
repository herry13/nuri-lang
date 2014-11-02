# Nuri Language Compiler

[![Build Status](https://travis-ci.org/nurilabs/nuri-lang.svg?branch=master)](https://travis-ci.org/nurilabs/nuri-lang)


## Table of Contents

1. [Introduction](#intro)
2. [To Build](#build)
3. [Usage](#usage)


<a name="intro"></a>
## Introduction

The Nuri language serves two objectives:

1. To specify a declarative specification of configuration state of a system.
2. To model a reconfiguration task which consists of:
    - an initial state
    - a goal state
    - a set of actions
    - global constraints

This compiler can:
- syntax checking
- type checking
- evaluate the global constraints over the specification
- generate a compilation result in JSON
- automatically generate a plan of a reconfiguration (planning) problem -- this requires additional other programs

This compiler is used by the [Nuri](https://github.com/nurilabs/nuri) configuration management tool.


<a name="build"></a>
## To Build

Requirements:
- OCaml version >= 3.12.1
- OPAM version >= 1.0.0
- Yojson version >= 1.1.8

Compilation:
```bash
cd ocaml
make
```

The above commands generates file `nuric`. Notes that in default the codes are compiled into static-native binary file. However, we can compile it into OCaml bytecodes by setting variable `NATIVE=0` in `Makefile`.


<a name="usage"></a>
## Usage

The simplest way is:

    $ nuric spec.nuri
    
This will parse the specification in file `spec.nuri`, perform the type-checking, evaluate the global constraints, and finally generate a compilation result in JSON.

Other options:
- `-x` : perform only syntax-checking
- `-t` : perform syntax and type-checking
- `-m` : return `root` instead of object `main`
- `-g` : do not evaluate the global constraints
- `-p` : solve a reconfiguration (planning) problem

