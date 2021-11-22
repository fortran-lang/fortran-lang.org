---
layout: book
title: compiler_options
permalink: /learn/intrinsics/f_compiler_options
---
### NAME

**compiler\_options**(3f) - \[COMPILER INQUIRY\]
Options passed to the compiler

### SYNTAX

str = **compiler\_options**()

### DESCRIPTION

compiler\_options returns a string with the options used for compiling.

### ARGUMENTS

None.

### RETURN VALUE

The return value is a default-kind string with system-dependent length.
It contains the compiler flags used to compile the file, which called
the compiler\_options intrinsic.

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

**compiler\_version**(3), **iso\_fortran\_env**(7)
