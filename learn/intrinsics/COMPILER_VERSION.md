---
layout: book
title: compiler_version
permalink: /learn/intrinsics/COMPILER_VERSION
---
### NAME

__compiler\_version__(3f) - \[COMPILER INQUIRY\] Compiler version string
(GFDL)

### SYNTAX

str = __compiler\_version__()

### DESCRIPTION

__compiler\_version__(3f) returns a string containing the name and
version of the compiler.

### ARGUMENTS

None.

### RETURN VALUE

The return value is a default-kind string with system-dependent length.
It contains the name of the compiler and its version number.

### EXAMPLE

Sample program:

```
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

Example results:

```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```

### STANDARD

Fortran 2008

### CLASS

Inquiry function of the module \[\[iso\_fortran\_env\]\]

### SEE ALSO

__compiler\_options__(3), __iso\_fortran\_env__(7)
