---
layout: book
title: dble
permalink: /learn/intrinsics/DBLE
---
## __Name__

__dble__(3) - \[TYPE:NUMERIC\] Double conversion function
(GFDL)

## __Syntax__
```fortran
result = dble(a)
```
## __Description__

__dble(a)__ Converts __a__ to double precision real type.

## __Arguments__

  - __a__
    : The type shall be _integer_, _real_, or _complex_.

## __Returns__

The return value is of type _doubleprecision_.

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
  Results:
```text
  2.1800000667572021  5.0000000000000000   2.2999999523162842     
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__float__(3)](FLOAT),
[__real__(3)](REAL)

###### fortran-lang intrinsic descriptions
