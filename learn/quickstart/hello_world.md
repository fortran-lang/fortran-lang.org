---
layout: book
title: Hello World
permalink: /learn/quickstart/hello_world
---

In this part of the tutorial, we will write our first Fortran program:
the ubiquitous _Hello World_ example.

However before we can write our program, we need to ensure that we have
a Fortran compiler set up.

{% include note.html content="Fortran is a <b>compiled language</b> which means that once written, the source code must be passed through a
compiler to produce a machine executable that can be run." %}

## Compiler setup

In this tutorial, we'll work with the free and open source 
[GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/), 
which is part of the 
[GNU Compiler Colection (GCC)](https://gcc.gnu.org/).

To install gfortran on Linux, use your system package manager.
On macOS, you can install gfortran using [Homebrew](https://brew.sh/) or [MacPorts](https://www.macports.org/).
On Windows, you can get native binaries [here](http://www.equation.com/servlet/equation.cmd?fa=fortran).

To check if you have _gfortran_ setup correctly, open a terminal and run the following command :

```shell
$> gfortran --version
```

this should output something like:

```
GNU Fortran 7.5.0
Copyright (C) 2017 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

## Hello world

Once you have setup up your compiler, open a new file in your favourite code editor and enter the following:

```fortran
program hello
  ! This is a comment line, it is ignored by the compiler
  print *, 'Hello, World!'
end program hello
```

Having saved your program to `hello.f90`, compile at the command line with:
```shell
$> gfortran hello.f90 -o hello
```

{% include note.html content=".f90 is the standard file extension for modern fortran source files.
The 90 refers to the first modern fortran standard in 1990." %}

To run your compiled program:
```shell
$> ./hello
Hello, World!
```

Congratulations you've written, compiled and run your first Fortran program!
In the next part of this tutorial we will introduce variables for storing data.
