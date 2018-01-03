# Syntax Examples

This document provide a list of code snippets in a structured manner in order to provide a basic overview of the language. It will also display a set of features that the language will have built-in that will solve specific issues related to low-level programming requirements.

## Values and Variables

The language will provide basic features found in any general-purpose languages. Basic value types will built-in to represent simple to express data.

### Mutable and Immutable values

There is support for both immutable and mutable values.

Values defined via `let` like `let x = 5` are **immutable**. `let` is **binding** a value of a certain type to a name. Because it is a binding, the value for named binding should be set only once.

```Fsharp
let x = 5
let y = 6
x <- 7      <- doesn't compile because x is immutable
```

Values defined via `var` like `var x = 5` are **mutable**. Because they can mutate over time, we decide to call them variables.

```Javascript
var e = 5
e <- 7
```

### Number

Support for primitive number are defines as follows:

- byte, int8
- uint16, int16
- uint32, int32
- uint64, int64
- float, double, fixed-point

_Note byte is unsigned_

```Fsharp
let (a:byte) = 17ub
let (b:int8) = 0xA1b

let x = 5
let y = 6
let z = x + y + 2*x*y           <- support for infix operators

let (x:float) = 7.2             <- support for shadowing
let (fp:fixed[4,12]) = 0xA.E15  <- support for some refinement types


// Doesn't compile because we have defined a fixed[4,12] and not a fixed[8,8]
let (fpFailed:fixed[8,8]) = 0xA.E1
```

### Strings

Support for string values.

- string
- char

```Fsharp
let (s1:string) = "Hello"
let (s2:char) = 'a'

let (s1':char []) = ['H';'e';'l';'l';'o']
s1' = s1            <- internally a string is a char[]
// val it: true
```

## Functions

Functions are basic elements of the language that contains a behavior to apply over a set of input data. They take elements from an input set and return an element of an output set. We consider them as first-class citizen of the language, and thus are as easily manipulable as simple values. They are the basic element of composition, and by composing them we build up more complex behavior.

### Function definition

The concept of function is defined in such way that allows support for first-class support.

The functions are **curried** by default, and thus allow us to **partially apply** them in order to generate a new function with fix parameters. A function has a type associated to it that we call a **signature**.

```elm
add : int -> int -> int
add x y =
    x + y

add3 : int -> int -> int -> int
add3 x y z =
    let tmp = x + y
    tmp + z

// Functions can be passed as parameters, such as values
addWithOperation : (int -> int) -> int -> int -> int
addWithOperation fn x y =
    (fn x) + (fn y)
```

### Nominal types

```
type test() =
    let five = 5
    var another = 6

    //instance function
    add: int -> int -> int
    add x y =
        x + y

    //static function
    add: int -> int -> int
    static add x y =
        x + y
```

### Anonymous functions

It is often the case that users want to pass functions as parameters to other functions but on the fly such that the function is never really defined. We called these kind of functions **anonymous** functions.

```elm
addWithOperation : (int -> int) -> int -> int -> int
addWithOperation fn x y =
    (fn x) + (fn y)

addWithSquare : int -> int -> int
addWithSquare x y =
    addWithOperation (fun z -> z * z) x y
```

## Tuples

TBD:
```
let (t: int * float * string) = (5, 2.0, "hello")
```

or 

```
let (t: int, float, string) = (5, 2.0, "hello")
```

## Algebraic Data Types

the support for ADT allow the user to manipulate/represent data in typed manner, where the type models real world data in a very clear and concise manner.

To represent any data, a logic-like semantic is needed with the 2 basic constructs which are :

- a *Product* -> **Records** ***OR*** **Tuples**
- a *Sum*     -> **Unions**

```FSharp
union Boolean =
    | True
    | False

union DivisionResult = Result (result:int) (rest:int)

//concrete record type
record Being =
    { Age: int }

//creates an anonymous record type with the fields x, y, z
let point = 
   { x = 3, y = 4, z = 5 }

//field access
let xCoord = point.x

isOlderThan: int -> Being -> bool
isOlderThan n being =
    being.Age > n

record Person =
    { Being with
      Name : string
    }

let person =
    { Age  = 5
      Name = "A Name"  }

let result = isOlderThan 10 person
//val result : false
```

Being is a record and same goes with Person, however the Person record is composed from the Being record. Therefore, it can apply any function defined for Being and also functions specific to Person.  

Two new keywords are defined : *`union`* and *`record`* to be explicit on the type of ADT we are using

*Records have structural inheritance*, therefore functions can be used for other records than the one specified in the function signature, if that record is built as the composition of that record and other fields.

Records also expose property functions so that a simple property extraction does not need an anonymous function defining:

```
let mappedRecord =
    [point,{x=0, y=0}]
    |> List.map .x
```

Rather than the verbose anonymous function syntax to extract a value:  

```
let mappedRecord =
    [point,{x=0, y=0}]
    |> List.map (fun x -> x)
```

## Extended Data Types

### Array type

An array type is a type that describes a collection of elements of the same types. These elements are accessible via indexes (identifying keys) that allow to manipulate those stored values.
The array type is dependent of the type of the elements inside it. If we were to have an array of ints then we would have the following types :

```FSharp
// the type of Array could be theoretically defined as an inductive list  // via the use of unions :
union Array a =
    | Empty
    | Cons a (Array a)

type ArrayOfInts = Array int    <- type aliasing
```

As we can see Array is dependent of the type 'a which can be anything.
In fact, we just took the occasion to show the usage of **generics** as being equivalent to defining *single case union* with a generic parameter of that union case (which constructor would internally be a function at the type level, allowing partial application over union cases).

### Contracts (***to be defined later : based on traits and protocols***)

### Session types + linear types + typestate definitions (***to be defined later***)

Session types + linear types + typestate definitions, or how to provide **first-class support for concurrency** in the language at the same level as functions. I will described that in the future if we can get to that, but that would be a unique feature to have!!!

## Control Flow

### Pattern Matching

#### Type matches
#### union matches

### Active matches
F# has a notion of [Active patterns](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns) 

We can define what is called an active match which is a function that expects the target type followed by any other parameters which will result in a boolean result.

```fsharp
record Soldier =
    { hp: int
      agility: int
      strength: int }

    hp: Soldier -> int -> bool
    hp value pattern =
        pattern = value.hp

let soldier = Soldier(hp: 99, x: 10, y: 10)

match soldier with
    .hp 0: -> print "dead soldier"
    _: -> print "alive soldier"

val : "alive soldier"
```

### If Then Else

### loops

#### while loops

#### for loops (maybe opt for purely iterations ? maybe not?)

#### iteration loops over collections (maybe ?)

## Modules and Namespaces



## Notation extension

See: http://docs.idris-lang.org/en/latest/tutorial/syntax.html

## Constraints

Constraints can be applied to all primitives, lets take string as an example:

string< P >

`string < P >` is a kind of string that is defined via a predicate `P` : `string -> bool`, over the string. This is part of the refinement type system, that will be developed by adding a mechanism of constraints over values.

## reference counting/tracking, allocation, deallocation

## Overall syntax historic

explain that it is an ML based syntax, light with no curly braces and all that stuff. Maybe to put at the beginning ?
