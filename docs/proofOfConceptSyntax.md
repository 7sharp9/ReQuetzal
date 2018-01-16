# Scope

In order to limit the scope and complexity at the beginning of the project we will be using a limited version of the language in order to get various parts of the compiler operational.  The following is a list of syntactical scope of the language followed by parse tree and AST samples from similar languages such as F#, we will use this as a vague reference point during the design.

## Primitive types

We will be limiting scope to `int`, `bool`, `string` and `float` to focus on getting simple type inference working before introducing variants like `unint32`, `double` etc.

## Bindings
Bindings will be available for `let` and mutable `var` bindings

## Functions
Functions will be available in both fully named and anonymous variants, I considered limiting the scope to only named functions but short function application will be a core part of the language and should be included.  

## Modules
Initial modules will be out of scope and binding, types and function will all be implicitly in a single global module.  We will explore module definition importing and exporting in a later phase.

## Records
Records comprising of primitive type can be defined but row polymorphic features will be beyond the initial scope.

## Unions
Simple unions with primitive components will be considered the only valid syntax initially

## Pattern matching
While pattern matching is an important part of the language we will not be exploring it in any detail in the first phase.

## Advances language concepts  

For version 0.1 of the language we will not be approaching any of the advanced parts of the language like constraints, active matches, and notation extension.  

# Syntax examples 

## 1: Simple let and var bindings

### 1.1 simple immutable bindings
```fsharp
let x = 42
let y = x + 43
let z = x + y
```
### 1.2 simple mutable bindings
```
var a = 42
var b = a + 43
var c = a + b

a <- b + c
```

### parse tree
### AST

## Functions
### 2.1 simple named function
elm
```
id: 'a -> 'a
id a = a
```

```elm
add: int -> int -> int
add a b =
    a + b
```
### 2.2 simple partially applied function
```elm
addTwo: int -> int
addTwo =
    add 2
```

### parse tree
### AST

## 3: Records
### 3.1 simple record definition and instantiation
```fsharp
type Point = {x: int, y: int}
let myPoint = { x = 42, y = 172 }
```

### 3.2 records with mixed types
```fsharp
type Person {
    name: string
    age: int
    height: float
}
let person1 = {name="Fred", age=28, height=5.8}
```

### 3.3 updating records
```fsharp
let anotherPoint1 = { myPoint with y = 53 }
//or syntax 2
let anotherPoint2 = { myPoint | y = 54 }
//or syntax 3
let anotherPoint2 = { myPoint: y = 55 }
```

### 3.4 anonymous records

```elm
let anonymousPoint = { x = 42, y = 172 }
```

### parse tree
### AST

## 4: Unions

```fsharp
//syntax1 F# style
type Shape =
    | Rectangle of width: float * length: float
    | Circle of radius: float
    | Prism of width: float * float * height: float
let rect = Rectangle(length = 1.3, width = 10.0)
let circ = Circle (1.0)
let prism = Prism(5.0, 2.0, height = 3.0)

//syntax2 nameless partials
type Shape =
    | Rectangle of float float
    | Circle of float
    | Prism of float float float
let rect = Rectangle 1.3 10.0
let circ = Circle 1.0
let prism = Prism 5.0 2.0 3.0

//syntax3 named partials
type Shape =
    | Rectangle of (width: float)  (length: float)
    | Circle of radius: float
    | Prism of (width: float) float (height: float)
let rect = Rectangle (length = 1.3) (width = 10.0)
let circ = Circle (1.0)
let prism = Prism 5.0 2.0 (height = 3.0)
```  

We can use vary the syntax by not using of, perhaps using `:` instead.  
```fsharp
//syntax 4
type Shape =
    | Rectangle: width: float * length: float
    | Circle: radius: float
    | Prism: width: float * float * height: float
```
We could use use `,` to separate parameters and enclose in parentheses rather than using `of` as a prefix:  
```swift
//syntax 5
type Shape =
    | Rectangle(width: float, length: float)
    | Circle(radius: float)
    | Prism(width: float * float * height: float)
```


### parse tree
### AST
