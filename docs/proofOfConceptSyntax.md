# Scope

In order to limit the scope and complexity at the beginning of the project we will be using a limited version of the language in order to get various parts of the compiler operational.  The following is a list of syntactical scope of the language followed by parse tree and AST samples from similar languages such as F#, we will use this as a vague reference point during the design.

## Primitive types

We will be limiting scope to `int`, `bool`, `string` and `float` to focus on getting simple type inference working before introducing variants like `unint32`, `double` etc.

## Binding
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

