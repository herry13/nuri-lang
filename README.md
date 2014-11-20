# Nuri Language Compiler

[![Build Status](https://travis-ci.org/nurilabs/nuri-lang.svg?branch=master)](https://travis-ci.org/nurilabs/nuri-lang)
[![Gem Version](https://badge.fury.io/rb/nuric.png)](https://badge.fury.io/rb/nuric)


## Table of Contents

1. [Introduction](#intro)
2. [To Build](#build)
3. [Install](#install)
4. [Usage](#usage)
5. [Nuri Language](#nuri-language)
6. [Planning for Orchestrating Configuration Changes](#planning)
8. [JSON to Nuri](#json2nuri)
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

This gem also provides a Ruby wrapper of the Nuri compiler so that you can call the compiler/planner through any Ruby program.

TODO: A Python wrapper is under development.


<a name="usage"></a>
## Usage

The simplest way is:
```bash
nuric spec.nuri
```

This will parse the specification in file `spec.nuri`, perform the type-checking, evaluate the global constraints, and finally generate a compilation result in JSON.

Other options:
- `-x` : the compiler will only perform syntax-checking over the specification;
- `-t` : the compiler will only perform syntax-checking and type-checking over the specification, and then print the type of every variable;
- `-m` : the compiler will perform syntax-checking, type-checking, determine the final value of every variable, evaluate the global constraints, but it will use object `root` as the final output instead of object `main`;
- `-i` : the compiler will evaluate specification from standard input (STDIN) instead of from a file;
- `-g` : the compiler will perform syntax-checking, type-checking, determine the final value of every variable, but it will not evaluate the global constraints;
- `-p` : the compiler will solve a planning (reconfiguration) problem which requires two input files (one is the initial state, and second is the goal state).
 
Notes that `-p` option requires an external program that is not included in this repository. You can contact the author ([Herry](mailto:herry13@gmail.com)) or use a binary distribution through Rubygems (see [Install](#install)).


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

The above specification declares two new schemas i.e. `PM` and `VM`. `PM` extends schema `Machine`, thus it inherits all `Machine`'s attributes (`name` and `running`) and their default values. `VM` also extends schema `Machine`, but besides attributes `name` and `running`, it also has a new attribute i.e. `is_on` with type reference of `PM`. So `is_on` can only be assigned with a reference of an object that implements schema `PM` or other sub-schemas of `PM`.

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
## Planning for Orchestrating Configuration Changes

Nuri language can be used to specify a planning problem of reconfiguration i.e. a problem to generate a workflow that can bring the system from its current state to a particular desired state. Thus, to specify the problem, we need to have two descriptions in two separate files: first is the description of the current state of the system (_initial state_), and second is the description of the desired state of the system (_goal state_).

The following examples show how to specify the planning problem in Nuri language and then solve it using the Nuri compiler.


### Example 1 : Service Reference

[![Example 1](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a.png)

In the above figure, assume that we have two services `service1` and `service2`, and a `client`. The current state of the system (**left side**) is that `service1` is running, `service2` is stopped, and the `client` is referring to `service1`. Due to particular reason (e.g. maintenance on `service1`), we would like to change the configuration of the system as illustrated on the **right side** where `service1` is stopped, `service2` is running, and the `client` is referring to `service2`. However, we have a particular global constraints that should be maintained throughout configuration changes i.e. the `client` should always refer to a running service.

#### Schemas

The first step to model the planning problem is to model resources through schemas.

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

The above specification is in file `schemas.nuri`. It has an _enum_ `State` that has two symbols i.e. `stopped` and `running`. It also has two schemas. First is `Client` that has attribute `refer` that holds a reference of a service. Schema `Client` has an action `redirect` which redirects the reference to any service. The second schema is `Service` that has attribute `state` whose default value is `State.stopped`. It has two actions i.e. `start` and `stop`.


#### Current State

```java
// file initial.nuri
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
  client2 extends client1  // use client1 as prototype
}
```

The above specification models the current state of the system where all resources are inside object `main`. Object `service1` and `service2` are implementing schema `Service`. Thus, they are inheriting their schema attribute and actions. Attribute `state` of `service1` and `service2` has value `State.running` and `State.stopped` respectively. On the other side, object `client1` is implementing schema `Client` where attribute `refer` is equal to `service1` (`client1` is referring `service1`). While object `client2` is using `client1` as its prototype (this copies all attributes and actions of `client1` to `client2`).


#### Desired State

```java
// file : goal.nuri
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
  
  // global constraints
  global {
    client1.refer.state = State.running;
    client2.refer.state = State.running;
  }
}
```

The above specification defines the desired state of the system where `service1` is stopped (`state = State.stopped;`), `service2` is running (`state = State.running;`), while `client1` and `client2` are referring `service2` (`refer = service2;`).

The global constraints are defined in scope `global` where in this case the constraint is a conjunction of two equality statements. The first statement i.e. `client1.refer.state = State.running;` states that `client1` must always refer to a running service. The second statement i.e. `client2.refer.state = State.running;` states that `client2` must always refer to a running service. Since the constraint is a conjunction of logic formula, then the order is insignificant.


#### Solution Plan

The specification of the current state is defined in file `initial.nuri`, and the specification of the desired state is defined in file `goal.nuri`. To generate the plan, we can invoke the Nuri compiler using option `-p` as follows:

```bash
nuric -p initial.nuri goal.nuri
```

Notes that the plan will only be generated if the solution does exist. If not, then the program will return a non-successful exit flat.

The above command will generate a plan in JSON format as the following.

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

Notice that the actions of the plan is in an array of attribute `actions`. The order of the actions are the order of the solution sequential plan i.e.:

1. `service2.start ()`
2. `client2.redirect (s = service2)`
3. `client1.redirect (s = service2)`
4. `service1.stop ()`

Notice that each action of the above plan has attribute `before` and `after` with an array of integers. These two attributes represent the partial-ordering constraints between the action with other actions where the integers are the indexes of other actions.

Let us focus on the second action i.e. `client2.redirect`:

```json
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
}
```

The above action descriptions shows that the `name` of the action is `client2.redirect` (action `redirect` owned by object `client2`). It has a single parameter `s` with value `$service2` -- the value is a reference to object `service2` (in JSON, every Nuri reference is represented as a string started with `$` character). The `cost` of the action is `1`. The `conditions` before execution is `service2.state = State.running`. The `effects` after execution is `client2.refer = service2`. `before` gives a list of action indexes that must be successfully executed before this action -- in this case, action `service2.start` that has index `0` must be successfully executed before executing action `client2.redirect`. `after` gives a list of action indexes that should be executed after this action has been successfully executed -- in this case, action `service1.stop`.

By using partial-ordering constraints which are defined in `before` and `after` of all actions, the following partial-order plan can be executed using a parallel execution algorithm in order to decrease the execution time.

[![Solution Plan of Example 1](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a-plan.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system1a-plan.png)



### Example 2 : Continuous Deployment

[![Example 2](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3.png)

The above figure illustrates a common continuous deployment problem. Assume we have two identical 2-tier systems, one (`service1{a,b}`) is the primary and another (`service2{a,b}`) is the backup. The first layer of the system `service1a` and `service2a` depend on the service at the second layer i.e. `service1b` and `service2b` respectively. Currently, we have a `client` that refers to the primary system (`service1{a,b}`).

Due to a bug, we have to upgrade the second layer services (`service1b` and `service2b`) from version `1` to version `2`. However, we do not want disrupting the client. In other words, we want the client to always using a running service.

#### Schemas

We can reuse the schemas that have been used in the first example. However, we need to modify schema `Service` in order to add attribute `version` and action `upgrade`.

```java
// file : schemas.nuri
enum State {
  ...
}

schema Client {
  ...
}

schema Service {
  ...
  
  version : int = TBD;
  
  def upgrade (ver : int) {
    condition {
      this.state = State.stopped;
    }
    effect {
      this.version = ver;
    }
  }
}
```

As shown above, we add attribute `version` with type of `int`. This attribute represents the version of the service. The default value is `TBD`, which means that it must be replaced with any integer value.

We also add action `upgrade` that models the upgrade process of the service from one to another version. The action has a parameter `ver` which is the target upgrade version. The action has a precondition i.e. the service must be stopped. While the effect is that the service has been upgraded to the version specified in the parameter.

Note that the compiler will automatically set the parameter with any possible value. Thus, we do not need to define the value manually.

#### Current State

```java
// file : initial.nuri
import "schemas";

main {
  service1a isa Service {
    state = State.running;
    version = 1;
  }
  service1b isa Service {
    state = State.running;
    version = 1;
  }
  service2a isa Service {
    state = State.running;
    version = 1;
  }
  service2b isa Service {
    state = State.running;
    version = 1;
  }
  client isa Client {
    refer = service1a;
  }
}
```

The above specification describes that the current state of the system. The primary system has two services i.e. `service1a` and `service1b`, while the backup system also has two services i.e. `service2a` and `service2b`. All services are running and they have version `1`. We also have a `client` that is referring to `service1a` which is the first layer of the main system.


#### Desired State

```java
// file : goal.nuri
import "schemas";

main {
  service1a isa Service {
    state = State.running;
    version = 1;
  }
  service1b isa Service {
    state = State.running;
    version = 2;  // upgrade to version 2
  }
  service2a isa Service {
    state = State.running;
    version = 1;
  }
  service2b isa Service {
    state = State.running;
    version = 2;  // upgrade to version 2
  }
  client isa Client {
    refer = service1a;
  }
  
  // global constraints
  global {
    if service1a.state = State.running; then service1b.state = State.running;

    if service2a.state = State.running; then service2b.state = State.running;

    client.refer.state = State.running;
  }
}
```

The above specification describes the desired state of the system where there are only two changes i.e. the version of `service2a` and `service2b` is changed to `2` (previously `1`). Other attributes have the same value including the client where it is referring to `service1a`.

We have three global constraints. The first states that `service1a` depends on `service1b`. The second states that `service2a` depends on `service2b`. And the third states that the `client` always refers to a running service.


#### Solution Plan

Invoking the same command i.e. `nuric -p initial.nuri goal.nuri`, we will get [this solution plan](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3-plan.json). The following graph illustrates the workflow of the plan.

[![Solution Plan of Example 2](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3-plan.png)](https://raw.githubusercontent.com/nurilabs/nuri-lang/master/examples/system3-plan.png)



<a name="json2nuri"></a>
## JSON to Nuri

The Nuri compiler compiles every specification into an intermediate representation in JSON format. However, sometimes we want to revert back from JSON to Nuri format. This capability is very useful for reverse engineering or generating the current/goal state from a program.

[nuri2json](https://github.com/nurilabs/nuri-lang/blob/master/utils/json2nuri.rb) provides this capability. The script will receive a JSON data from given file or standard input (STDIN). It converts the JSON data into a Nuri representation and then print the output to standard output (STDOUT).

If we have a JSON specification in file `spec.json`, then the following command will convert it into Nuri representation:

```bash
json2nuri.rb spec.json
```

If we want to give the JSON specification through standard input, then use the following command:

```bash
cat spec.json | json2nuri.rb -i
```



<a name="license"></a>
## License

[Apache License 2.0](https://github.com/nurilabs/nuri-lang/blob/master/LICENSE).
