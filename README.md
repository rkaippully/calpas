# Calpas - A Pascal compiler and VM

[![Hackage](https://img.shields.io/hackage/v/calpas)](https://hackage.haskell.org/package/calpas)
[![Build Status](https://img.shields.io/github/workflow/status/rkaippully/calpas/Haskell%20CI/master)](https://github.com/rkaippully/calpas/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

Calpas is a compiler and virtual machine for [Pascal
programs](https://en.wikipedia.org/wiki/Pascal_(programming_language)). Calpas implements the Pascal language as defined
by [The Programming Language Pascal (Revised Report) 1973
standard](https://www.research-collection.ethz.ch/handle/20.500.11850/68910) with some minimal extensions. It compiles
Pascal programs to virtual machine p-code. The Calpas VM executes this p-code to run programs.

```shell
$ calpasc -o test.cpc test.pas    # Compile to p-code
$ calpas test.cpc                 # Run a program
```

## Changes from Standard

- Supports `(* *)` and `/* */` style comments
- Record field lists can be empty and can have trailing semicolon for the field list before "end"
- Case list elements can have a trailing semicolon before "end"
- Procedures and functions as parameters are not supported
