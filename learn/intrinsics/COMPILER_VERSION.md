---
layout: book
title: compiler_version
permalink: /learn/intrinsics/COMPILER_VERSION
---
## __Name__

__compiler\_version__(3) - \[COMPILER INQUIRY\] Compiler version string
(GFDL)

## __Syntax__

str = __compiler\_version__()

## __Description__

__compiler\_version__(3) returns a string containing the name and
version of the compiler.

## __Arguments__

None.

## __Returns__

The return value is a default-kind string with system-dependent length.
It contains the name of the compiler and its version number.

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

[__compiler\_options__(3)](COMPILER_OPTIONS),
__iso\_fortran\_env__(7)

###### fortran-lang intrinsic descriptions
