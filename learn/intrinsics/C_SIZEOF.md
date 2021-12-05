---
layout: book
title: c_sizeof
permalink: /learn/intrinsics/C_SIZEOF
---
## __Name__

__c\_sizeof__(3) - \[ISO\_C\_BINDING\] Size in bytes of an expression
(GFDL)

## __Syntax__

n = __c\_sizeof__(x)

## __Description__

__c\_sizeof__(x) calculates the number of bytes of storage the
expression X occupies.

## __Arguments__

  - __X__
    : The argument shall be an interoperable data entity.

## __Returns__

The return value is of type integer and of the system-dependent kind
c\_size\_t (from the \[\[iso\_c\_binding\]\] module). Its value is the
number of bytes occupied by the argument. If the argument has the
POINTER attribute, the number of bytes of the storage area pointed to is
returned. If the argument is of a derived type with POINTER or
ALLOCATABLE components, the return value does not account for the sizes
of the data pointed to by these components.

## __Examples__

Sample program:

```fortran
program demo_c_sizeof
use iso_c_binding
implicit none
real(c_float) :: r, s(5)
   print *, (c_sizeof(s)/c_sizeof(r) == 5)
end program demo_c_sizeof
```

The example will print .true. unless you are using a platform where
default _real_ variables are unusually padded.

## __Standard__

Fortran 2008

## __See Also__

[__storage\_size__(3)](STORAGE_SIZE)
