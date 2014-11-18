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

Nuri is a static-typed language with a powerful type-inference (the compiler can determine the variable's type based on its value). It supports common types i.e. `bool`, `int`, `float`, `string`, `object`, _reference_, and _array_. It also allows two user-defined types i.e. _schema_ and _enum_.

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

There are 4 basic types i.e. `bool`, `int`, `float`, and `string`. The type of a variable is determined by its first declaration. We can let the compiler to automatically infer the type of the variable based on its value, or explicitly declare the type.

```java
main {
  a : bool = true;  // type: bool
  b = 1;            // type: int
  c = 2.3;          // type: float
  d = "a string";   // type: string
  c = 4;            // type: float
}
```

The above specification shows that the type of `a` is explicitly declared as `bool`. While the types of `b`, `c`, and `d` are automatically inferred by the compiler i.e. `int`, `float`, and `string` respectively. The last statement (`c = 4;`) is accepted because any float variable can be assigned with any integer value, but not vice versa.

Nuri allows us to define an object, which is very useful to model a resource. Basically, all objects have type `object`. However, we can use _schema_ to defined a custom type of object -- more details about _schema_ will be described in the following subsection. A variable also can have a type of _reference_ of _object_ or _schema_, which is a natural way to express dependency between resources.

Array is an abstract data structure supported by Nuri. For simplicity, we can only have an array of `bool`, `int`, `float`, `string`, or reference. An array can be in any size of dimensions where the type of an n-dimension array is compatible with an m-dimension array.

There is another way to define a custom type i.e. using _enum_. _Enum_ allows us to define a set of symbols that can only be used by particular variable.

```java
enum State { stopped, running }  // enum type with two symbols: stopped & running
main {
  a {
    ipaddress = "10.0.0.1";
    users = ["herry", "paul"];
    apache {
      state = State.running;
    }
  }
}
```

The above specification defines an _enum_ type `State` which has two symbols i.e. `stopped` and `running`. It has one machine `a`. The machine has an IP address `10.0.0.1`. It has two users i.e. `herry` and `paul`. It has an apache server whose state is `running`. Note that the compiler automatically infers variable `state` to having type `enum State`.


#### Schema

Schema is similar to class on programming languages. However, it is more flexible in the sense that any object that implement a schema may have attributes that are not exist in the schema's definition. Thus, we do not have to define a new schema for every specific object. A schema is very useful to group objects (resources) that have similar properties, for example: `Machine`, `File`, `Service`.

```java
schema Machine {
  name = "";
  running = false;
}
```

You can define a schema by declaring its name and attributes. The above is an example of schema `Machine` which has two attributes i.e. `name` (type `string`) and `running` (type `bool`), where their default values are an empty string (`""`) and `false` respectively. Note that the Nuri compiler uses type-inference technique to determine the type of `name` and `running`, where in this case their types are obtained from the values.

An attribute can be assigned with `TBD` (_To Be Defined_) value, which means that we or other users should replace this value with a non `TBD` value later. The compiler will ensure that the final output does not have variable with `TBD` value. If such situation exists, then an error will arise.

### Inheritance and Reference

```java
schema PM extends Machine { }
schema VM extends Machine {
  is_on : *PM = null;
}
```

The above specification declares two new schemas i.e. `PM` and `VM`. `PM` extends schema `Machine`, thus it inherits all `Machine`'s attributes (`name` and `running`) and their default values. `VM` also extends schema `Machine`, but besides it has attributes `name` and `running`, it also has a new attribute i.e. `is_on` with type reference of `PM`. So `is_on` can only be assigned with a reference of an object that implements schema `PM` or other sub-schemas of `PM`.

```java
schema Service {
  name = "";
  running = false;
}
schema Client {
  refer : *Service = null;
}
```

The above specification is another example where schema `Client` has an attribute `refer` whose type is a reference of `Service` where its default value is `null`.


### Declarative Specification of Configuration State

```java
import "schemas"; // import file schemas.nuri that contains schema:
                  // 'machine', 'PM', 'VM', 'Service', and 'Client'
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
    refer = vm1.httpd;
  }
}
```

The above specification is an example a configuration state of a system in Nuri language. The specification defines three objects that represent three machines i.e. `pm1`, `vm1`, and `pc`. Each of which inherits the attributes of their schema, where some attributes (e.g. `vm1.running`) inherits the default value (`false`), while others (e.g. `pm1.running`) has a new value (`true`).

Notice that the objects are inside another object i.e. `main`. This is a necessary because the compiler will ignore any declaration outside `main` (only variables inside `main` will be in the final output). This is very useful whenever you want to have objects/variables which act as prototypes of the others.


### Action = Configuration Change

An action describes a declarative specification of a configuration change. It has preconditions (`conditions`) i.e. the constraints that must be satisfied before executing the action, and postconditions (`effects`) i.e. the conditions after executing the action. An action can also have a set of typed-parameters, and a cost to express the preferences.

We can explicitly add actions to an object. However, the best practice is to define the actions inside a schema so that every object can inherit the actions through schema. This will allow us to implement _design pattern_ in our specification.

```java
schema Service {
  name = "";
  running = false;

  def start {
    conditions { this.running = false; }
    effects    { this.running = true;  }
  }
  def stop {
    conditions { this.running = true;  }
    effects    { this.running = false; }
  }
}
	
schema Client {
  refer : *Service = null;
	
  def redirect ( s : Service ) {
    conditions { s.running = true; }
    effects    { this.refer = s;   }
  }
}
```

The above specification shows that schema `Service` has two common actions i.e. `start` and `stop` which will be inherited by all objects that implement `Service`. Action `start` has a precondition where the object (referred by `this`) must be stopped (`this.running = false`). After execution, the action changes the object's state to running (`this.running = true`). A similar thing is applied to action `stop`.

Action `redirect` of schema `Client` has a parameter which is an object of `Service`. It has a precondition where the object `Service` (referred by `s`) must be running (`s.running = true`). After execution, the action changes the client object's state by setting attribute `refer` with value of a reference of the parameter value.



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
