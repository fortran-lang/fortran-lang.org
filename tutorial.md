---
layout: page
title: Fortran tutorial
permalink: /tutorial/
---

This page will include a Fortran tutorial.
It should be a no-nonsense practical step-by-step guide,
written in plain language.
It will include just enough information for a newcomer to Fortran to write
basic and correct Fortran programs and libraries, without external resources.
At a later time, we may decide to include more advanced topics like 
parallelism, Fortran OO, or C-interop.

Syntax highlighting doesn't work yet, it's a TODO. 

## Get a Fortran compiler

In this tutorial, we'll work with the free and open source 
[GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/), 
which is part of the 
[GNU Compiler Colection (GCC)](https://gcc.gnu.org/).

To install gfortran on Linux, use your system package manager.
Otherwise, for macOS or Windows, refer to gfortran binaries from 
[this page](https://gcc.gnu.org/install/binaries.html).

## Your first Fortran program

Here's your first Fortran program that prints some text to the terminal:

```fortran
program hello
  print *, 'Hello, World!'
end program hello
```

## Data types

Fortran comes with 5 built-in data types:

* `integer`--for data that represent whole numbers, positive or negative
* `real`--for floatin-point data
* `complex`--tuples of real numbers
* `character`--for text data
* `logical`--for data that represent boolean (true or false) values

### Declaration

## Arrays

## Procedures

### Functions

Here's a function that returns a cube-root of a real number:

```fortran
real function cbrt(x)
  real, intent(in) :: x
  cbrt = x**(1. / 3)
end function cbrt
```

### Subroutines

## Modules

etc.
