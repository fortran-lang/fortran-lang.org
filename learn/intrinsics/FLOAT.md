---
layout: book
title: float
permalink: /learn/intrinsics/FLOAT
---
## __Name__

__float__(3) - \[NUMERIC:TYPE\] Convert integer to default real
(GFDL)

## __Syntax__

result = __float__(a)

## __Description__

__float__(a) converts the _integer_ __a__ to a default real value.

## __Arguments__

  - __a__
    : The type shall be _integer_.

## __Returns__

The return value is of type default _real_.

## __Examples__

Sample program:

```fortran
program demo_float
implicit none
integer :: i = 1
   if (float(i) /= 1.) stop ' FLOAT FAILED'
end program demo_float
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__dble__(3)](DBLE),
[__real__(3)](REAL)
