---
layout: book
title: digits
permalink: /learn/intrinsics/DIGITS
---
## __Name__

__digits__(3) - \[NUMERIC MODEL\] Significant digits function
(GFDL)

## __Syntax__

result = __digits__(x)

## __Description__

__digits__(x) returns the number of significant digits of the internal
model representation of X. For example, on a system using a 32-bit
floating point representation, a default real number would likely return
24.

## __Arguments__

  - __X__
    The type may be _integer_ or _real_.

## __Returns__

The return value is of type _integer_.

## __Examples__

Sample program:

```fortran
    program demo_digits
    implicit none
        integer :: i = 12345
        real :: x = 3.143
        doubleprecision :: y = 2.33d0
        print *,'default integer:        ', digits(i)
        print *,'default real:           ', digits(x)
        print *,'default doubleprecision:', digits(y)
    end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

## __Standard__

Fortran 95 and later
