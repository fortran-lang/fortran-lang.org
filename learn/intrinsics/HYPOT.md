---
layout: book
title: hypot
permalink: /learn/intrinsics/HYPOT
---
## __Name__

__hypot__(3) - \[MATHEMATICS\] Euclidean distance function
(GFDL)

## __Syntax__

result = __hypot__(x, y)

## __Description__

__hypot__(x,y) is the Euclidean distance function. It is equal to

```
      sqrt{X__2 + Y__2}, without undue underflow or overflow.
```

## __Arguments__

  - __X__
    The type shall be _real_.

  - __Y__
    The type and kind type parameter shall be the same as X.

## __Returns__

The return value has the same type and kind type parameter as X.

## __Examples__

Sample program:

```
    program demo_hypot
    implicit none
      real(4) :: x = 1.e0_4, y = 0.5e0_4
      x = hypot(x,y)
    end program demo_hypot
```

## __Standard__

Fortran 2008 and later
