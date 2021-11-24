---
layout: book
title: c_sizeof
permalink: /learn/intrinsics/C_SIZEOF
---
### NAME

__c\_sizeof__(3f) - \[ISO\_C\_BINDING\] Size in bytes of an expression
(GFDL)

### SYNTAX

n = __c\_sizeof__(x)

### DESCRIPTION

__c\_sizeof__(x) calculates the number of bytes of storage the
expression X occupies.

### ARGUMENTS

  - __X__
    The argument shall be an interoperable data entity.

### RETURN VALUE

The return value is of type integer and of the system-dependent kind
c\_size\_t (from the \[\[iso\_c\_binding\]\] module). Its value is the
number of bytes occupied by the argument. If the argument has the
POINTER attribute, the number of bytes of the storage area pointed to is
returned. If the argument is of a derived type with POINTER or
ALLOCATABLE components, the return value does not account for the sizes
of the data pointed to by these components.

### EXAMPLE

Sample program:

```
   program demo_c_sizeof
   use iso_c_binding
   implicit none
   real(c_float) :: r, s(5)
      print *, (c_sizeof(s)/c_sizeof(r) == 5)
   end program demo_c_sizeof
```

The example will print .true. unless you are using a platform where
default REAL variables are unusually padded.

### STANDARD

Fortran 2008

### CLASS

Intrinsic function

### SEE ALSO

__storage\_size__(3)
