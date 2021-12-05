---
layout: book
title: dble
permalink: /learn/intrinsics/DBLE
---
## __Name__

__dble__(3) - \[NUMERIC:TYPE\] Double conversion function
(GFDL)

## __Syntax__

result = __DBLE__(A)

## __Description__

__DBLE__(A) Converts A to double precision real type.

## __Arguments__

  - __A__
    : The type shall be _integer_, _real_, or _complex_.

## __Returns__

The return value is of type DOUBLEPRECISION.

## __Examples__

Sample program:

```fortran
program demo_dble
implicit none
real:: x = 2.18
integer :: i = 5
complex :: z = (2.3,1.14)
   print *, dble(x), dble(i), dble(z)
end program demo_dble
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__float__(3)](FLOAT),
[__real__(3)](REAL)
