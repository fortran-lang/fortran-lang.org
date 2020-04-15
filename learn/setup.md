---
layout: page
title: Compiler setup
permalink: /learn/setup
---


Fortran is a compiled language which means that when written it must be passed through a
compiler to produce a machine executable that can be run.

This is in contrast to interpreted languages which can be run directly from the source file
using a special _interpreter_ program.

In this tutorial, we'll work with the free and open source 
[GNU Fortran compiler (gfortran)](https://gcc.gnu.org/fortran/), 
which is part of the 
[GNU Compiler Colection (GCC)](https://gcc.gnu.org/).

To install gfortran on Linux, use your system package manager.
Otherwise, for macOS or Windows, refer to gfortran binaries from 
[this page](https://gcc.gnu.org/install/binaries.html).

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

If this command works then you are ready to [write and compile your first Fortran program](/learn/beginner).
