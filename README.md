# Calpas - A Pascal compiler for JVM

[![Build Status](https://img.shields.io/github/workflow/status/rkaippully/calpas/CI/main)](https://github.com/rkaippully/calpas/actions?query=workflow%3A%22CI%22+branch%3Amain)

Calpas is a [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) compiler targeting the Java virtual
machine. Calpas implements the Pascal language as defined by [ISO/IEC 7185:1990
Pascal](https://web.archive.org/web/20160127044422/http://pascal-central.com/docs/iso7185.pdf) with some extensions. It
compiles Pascal programs to JVM bytecode.

```shell
$ calpasc PascalProg.pas    # Compile to PascalProg.class
$ java PascalProg           # Run the program
```

## Changes from Standard

- Supports `(* *)` and `/* */` style comments
- Record field lists can be empty and can have trailing semicolon for the field list before "end"
- Case list elements can have a trailing semicolon before "end"
- Procedures and functions as parameters are not supported
