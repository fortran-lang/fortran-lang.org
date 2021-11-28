---
layout: book
title: epsilon
permalink: /learn/intrinsics/EPSILON
---
## __Name__

__epsilon__(3) - \[NUMERIC MODEL\] Epsilon function
(GFDL)

## __Syntax__

result = __epsilon__(x)

## __Description__

__epsilon__(x) returns a nearly negligible number relative to 1.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of same type as the argument.

## __Examples__

Sample program:

```fortran
    program demo_epsilon
    implicit none
        real :: x = 3.143
        real(8) :: y = 2.33
        print *, epsilon(x)
        print *, epsilon(y)
    end program demo_epsilon
```

## __Standard__

Fortran 95 and later
