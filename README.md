![icon](https://raw.githubusercontent.com/vipentti/visp-fs/main/icon.png)

# visp-fs

A programming language inspired by the Scheme/Lisp/Clojure family, leveraging the power of F#.

## Introduction

`visp-fs` is an experimental programming language drawing inspiration from the Scheme/Lisp family. It transpiles into F# and incorporates some of the language features of F#. This language is currently under development and is not recommended for production use. The primary purpose of `visp-fs` is to serve as a learning tool for F# and to explore language design concepts.

## Features

- **Lisp/Scheme syntax**: Embrace the use of parentheses.
- **F# Interoperability**: Seamlessly interoperate with F# and .NET libraries.
- **LanguageServer**: A rudimentary Language Server Protocol (LSP) implementation supports the accompanying Visual Studio Code extension.

## Getting Started

### Prerequisites

Before you begin, ensure you have the following installed:

- [.NET 8.0 SDK](https://dotnet.microsoft.com/download/dotnet/8.0) or newer
- [F# Compiler](https://fsharp.org/use/windows/)

#### Optional

- [PowerShell Core](https://github.com/PowerShell/PowerShell) for helper script execution within the repository
- [NodeJS](https://nodejs.org/en) for building the VsCode extension

### Installation

To install `visp-fs`, clone the repository and build the project using the following commands:

```bash
git clone https://github.com/vipentti/visp-fs.git
cd visp-fs
dotnet build
```

The simplest way to execute `.visp` files is by using the provided PowerShell script in the repository root:

For example:

```powershell
.\cli.ps1 .\visp\tests\examples\variables-0.visp
```

## Examples

### Variables

Support for both immutable and mutable variables is included.

Immutable variables cannot be reassigned, but their internal mutable state, if any, can be modified.

```clojure
;; Define an immutable variable
(let value 1)
(printfn "value is %i" value)

;; Define a mutable variable
(mut valuemut 1)
(printfn "valuemut is %i" valuemut)

;; set! re-assigns the value pointed to by the first expression with the result of the second exprssion
;; in F# terms (set! varOrExpr value) becomes varOrExpr <- value
(set! valuemut (+ valuemut 1))

(printfn "valuemut is %i" valuemut)
```

### Functions

```clojure
;; create a function named hello
;; which takes one argument with the name 'name' and type 'string'
(fn hello ((name : string))
    ;; call function with the name printfn with the provided arguments
    (printfn "hello %s" name))

;; create another function which does the exactly the same thing as hello above
;; but only it uses type inference for the argument type
(fn hello-no-types (name)
    (printfn "hello %s" name))

;; function arguments & types may also be specified using brackets []
(fn hello-with-vector [[name : string]]
    (printfn "hello %s" name))

;; Create an anonymous lamda and store it in variable named 'anon'
(let anon (fn (name) (printfn "hello %s" name)))

(hello "test")
(hello-no-types "no types")
(hello-with-vector "vector")
(anon "anon")
;; Invoke an anonymous function directly
((fn (name)
    (printfn "hello %s" name)) "lambda")
```

For more examples of the language in action, see [visp/examples/aoc2023](https://github.com/vipentti/visp-fs/tree/main/visp/examples/aoc2023) which contains solutions for the Advent of Code puzzles. Additionally most of the files under [visp/](https://github.com/vipentti/visp-fs/tree/main/visp) are valid `visp-fs` programs.

## License

`visp-fs` is licensed under the [MIT License](https://github.com/vipentti/visp-fs/blob/main/LICENSE.md)
