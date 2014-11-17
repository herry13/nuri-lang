# Nuri Language Compiler

[![Build Status](https://travis-ci.org/nurilabs/nuri-lang.svg?branch=master)](https://travis-ci.org/nurilabs/nuri-lang)
[![Gem Version](https://badge.fury.io/rb/nuric.png)](https://badge.fury.io/rb/nuric)


## Table of Contents

1. [Introduction](#intro)
2. [To Build](#build)
3. [Install](#install)
4. [Usage](#usage)
5. [Nuri Language](#nuri-language)
6. [Planning](#planning)
7. [License](#license)


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


<a name="install"></a>
## Install

If you are familiar with Ruby, you can install a pre-compiled binary distribution through Rubygems:
```
sudo gem install nuric
```

To test, try:
```bash
nuric
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

**Nuri language** is a declarative configuration language which allows us to define three aspects of configuration:

1. a configuration state of the system,
2. a model of configuration change called as action,
3. global constraints, which are constraints that should be maintained during the changes.

Nuri adopts a prototype-object mechanism from [SmartFrog](http://smartfrog.org) where an object can be used as a prototype of other objects. This allows configuration composition through inheritance (`extends`). Nuri also supports a traditional composition through file inclusion (`import` and `include`).

Nuri is a static-typed language with a powerful type-inference (the compiler can determine the variable's type based on its value). It supports some basic types i.e. `bool`, `int`, `float`, `string`, `object`, reference, and list. It also allows two user-defined types i.e. **schema** and **enum**.

This section is a short introduction to Nuri language. It describes several aspects which are supported by the language with some examples.

### Main Object

In Nuri, the configuration of the target system must be defined inside object `main`. Other variables or objects defined outside this object `main` are treated as prototypes which will not be included in the final compilation output.

```java
x = 1;
main {
  a = 2;
  b = true;
  a = 3;
}
```

In the above specification we have object `main` and three variables i.e. `x`, `a`, and `b`. Since `x` is defined outside `main`, then it will not be included in the final output. On the other hand, since `a` and `b` are defined inside `main`, then they will be included in the final output. Thus, the final output after compilation is:

```java
a = 3;
b = true;
```

Notice that during compilation the value of `a` has been changed from `2` to `3`.


### Static Typing

TODO


### Schema

Schema is similar to class on programming languages. However, it is weaker in the sense that any object that implement a schema may have attributes that are not exist in the schema's definition. Schema is very useful to define objects that have similar properties, for example: `Machine` or `File`.

You can define a schema by declaring its name and attributes. Here is an example of schema `Machine` which has two attributes i.e. `name` (type `str`) and `running` (type `bool`). Where their default values are empty string (`""`) and `false` respectively.

```java
schema Machine {
  name = "";
  running = false;
}
```

Note that the Nuri compiler uses type-inference technique to determine the type of `name` and `running`, where in this case their types are obtained from the values.

An attribute can have a value of primitive type (`bool`, `int`, `float`, and `string`), a reference, a `null`, or an object. The reference value is similar with pointer in Java. However, once the variable has been defined, then it can only be assigned with a value whose type is the same or the subtype of the variable's type. For the above example, attribute `name` cannot be assigned with `true` since `bool` is not a subtype of `string`.

The attribute can be assigned with `TBD` (to be defined) value, which means that we or others should replace this value with other value (non `TBD`) later. The compiler will ensure that the final output does not have variable with `TBD` value. If such situation exists, then an error will arise.

### Inheritance and Reference

```java
schema PM extends Machine { }
schema VM extends Machine {
  is_on : *PM = null;
}
```

The above codes declare two new schemas i.e. `PM` and `VM`. `PM` extends schema `Machine`, thus it inherits all `Machine`'s attributes (`name` and `running`) and their default values. `VM` also extends schema `Machine`, but besides it has attributes `name` and `running`, it also has a new attribute i.e. `is_on` with type reference of `PM`. So `is_on` can only be assigned with a reference of an object that implements schema `PM`.

```java
schema Service {
  name = "";
  running = false;
}
schema Client {
  refer : *Service = null;
}
```

The above schemata are other examples that have an attribute with a reference type.


### Action

An action describes the mutation of attributes' value (effects) and the constraints (conditions) that have to be satisfied before execution. Each Nuri schema or object could have none or several actions. Each action may have parameters, cost, conditions, and effects.

```java
schema Service {
  name = "";
  running = false;

  def start {
    conditions {
      this.running = false;
    }
    effects {
      this.running = true;
    }
  }
}
	
schema Client {
  refer : *Service = null;
	
  def redirect ( s : Service ) {
    conditions {
      s.running = true;
    }
    effects {
      this.refer = s;
    }
  }
}
```

Action `start` of schema `Service` has a precondition where the object that implements the schema (referred by `this`) must be stopped (`this.running = false`). After execution, the action changes the object's state to running (`this.running = true`).

Action `redirect` of schema `Client` has a parameter which is an object of `Service`. It has a precondition where the object `Service` (referred by `s`) must be running (`s.running = true`). After execution, the action changes the client object's state by setting attribute `refer` with value of a reference of the parameter value.


### State

Nuri can be used to define a configuration state of a system. The following codes define three objects i.e. `pm1`, `vm1`, and `pc`. They inherit the attributes (and values) of their schemata, except the attributes which have been redefined. For example, object `pm1` inherits attributes `running` from schema `PM`. But instead of `false`, its value has been redefined with value `true`.

```java
import "schemata"; // import file schemata.nuri that contains all schemata
main {
  pm1 isa PM {
    name = "Physical Machine #1";
    running = true;
  }
  vm1 isa VM {
    name = "Virtual Machine #1";
    is_on = pm1;
    httpd isa Service {
      name = "HTTP Server";
    }
  }
  pc isa Client {
    refer = null;
  }
}
```

Notice that the objects are inside another object i.e. `main`. This **main** object is necessary because the compiler will ignore definition outside `main` (only variables inside `main` will be in the final output). This semantics is very useful whenever you want to have objects/variables which act as prototypes of the others.

In a reconfiguration task, you need to define two states: first is the current state, and second is the desired state of your system. By having these two states, and later actions and global constraints, the compiler can automatically generate a plan that can bring the system from the current to the desired state (under condition that the solution exists).


<a name="planning"></a>
## Planning

### Example 1 : Service Reference

[![Example 1](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a.png)

TODO: describe the above figure.


Model of resources:

```java
// file schemas.nuri
enum State {
    stopped,
    running
}

schema Client {
  refer: *Service = null;
  def redirect(s : Service) {
    condition { }
    effect {
      this.refer = s;
    }
  }
}

schema Service {
  state = State.stopped;
  def start {
    condition {
      this.state = State.stopped;
    }
    effect {
      this.state = State.running;
    }
  }
  def stop {
    condition {
      this.state = State.running;
    }
    effect {
      this.state = State.stopped;
    }
  }
}
```

Specification of the current state:

```java
import "schemas";

main {
  service1 isa Service {
    state = State.running;
  }
  service2 isa Service {
    state = State.stopped;
  }
  client1 isa Client {
    refer = service1;
  }
  client2 extends client1
}
```

Specification of the desired state:

```java
import "schemas";

main {
  service1 isa Service {
    state = State.stopped;
  }
  service2 isa Service {
    state = State.running;
  }
  client1 isa Client {
    refer = service2;
  }
  client2 extends client1
  global {
    client1.refer.state = State.running;
    client2.refer.state = State.running;
  }
}
```

The generated plan:

1. service2.start{}
2. client2.redirect{"s":"$service2"}
3. service2.stop{}

```json
{
  "type": "parallel",
  "version": 2,
  "actions": [
    {
      "name": "service2.start",
      "parameters": {
      },
      "cost": 1,
      "conditions": {
        "client1.refer": "$service1",
        "client2.refer": "$service1",
        "service1.state": "$State.running",
        "service2.state": "$State.stopped"
      },
      "effects": {
        "service2.state": "$State.running"
      },
      "before": [

      ],
      "after": [
        1,
        2,
        3
      ]
    },
    {
      "name": "client2.redirect",
      "parameters": {
        "s": "$service2"
      },
      "cost": 1,
      "conditions": {
        "service2.state": "$State.running"
      },
      "effects": {
        "client2.refer": "$service2"
      },
      "before": [
        0
      ],
      "after": [
        3
      ]
    },
    {
      "name": "client1.redirect",
      "parameters": {
        "s": "$service2"
      },
      "cost": 1,
      "conditions": {
        "service2.state": "$State.running"
      },
      "effects": {
        "client1.refer": "$service2"
      },
      "before": [
        0
      ],
      "after": [
        3
      ]
    },
    {
      "name": "service1.stop",
      "parameters": {
      },
      "cost": 1,
      "conditions": {
        "client1.refer": "$service2",
        "client2.refer": "$service2",
        "service1.state": "$State.running",
        "service2.state": "$State.running"
      },
      "effects": {
        "service1.state": "$State.stopped"
      },
      "before": [
        0,
        1,
        2
      ],
      "after": [

      ]
    }
  ]
}
```


### Example 2 : Continuous Deployment

[![Example 2](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3.png)

TODO: describe the above figure




<a name="license"></a>
## License

[Apache License 2.0](https://github.com/nurilabs/nuri-lang/blob/master/LICENSE).
