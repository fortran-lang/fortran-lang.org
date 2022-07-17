---
layout: book
title: compiler_options
permalink: /learn/intrinsics/COMPILER_OPTIONS
---
# COMPILER_OPTIONS
## __Name__

__compiler\_options__(3) - \[COMPILER INQUIRY\] Options passed to the compiler


## __Syntax__
```fortran
str = compiler_options()
```
## __Description__

compiler\_options returns a string with the options used for compiling.

## __Arguments__

None.

## __Returns__

The return value is a default-kind string with system-dependent length.
It contains the compiler flags used to compile the file, which called
the compiler\_options intrinsic.

## __Examples__

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
implicit none
   print '(4a)', &
      'This file was compiled by ', &
      compiler_version(),           &
      ' using the options ',        &
      compiler_options()
end program demo_compiler_version
```
Results:
```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```
## __Standard__

Fortran 2008

## __See Also__

[__compiler\_version__(3)](COMPILER_VERSION),
__iso\_fortran\_env__(7)

###### fortran-lang intrinsic descriptions
