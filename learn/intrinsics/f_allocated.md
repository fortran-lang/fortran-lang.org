---
layout: book
title: allocated
permalink: /learn/intrinsics/f_allocated
---
### NAME

**allocated**(3f) - \[ARRAY INQUIRY\] Status of an
allocatable entity

### SYNTAX

  - result = **ALLOCATED**(ARRAY)

  - result = **ALLOCATED**(SCALAR)

### DESCRIPTION

**ALLOCATED**(ARRAY) and **ALLOCATED**(SCALAR) check the allocation
status of ARRAY and SCALAR, respectively.

### ARGUMENTS

  - **ARRAY**
    the argument shall be an ALLOCATABLE array.

  - **SCALAR**
    the argument shall be an ALLOCATABLE scalar.

### RETURN VALUE

The return value is a scalar LOGICAL with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

### EXAMPLE

Sample program:

```
    program demo_allocated
    implicit none
    integer :: i = 4
    real(4), allocatable :: x(:)
       if (allocated(x) .eqv. .false.) allocate(x(i))
    end program demo_allocated
```

### STANDARD

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

### CLASS

Inquiry function

### SEE ALSO

**move\_alloc**(3)
