---
layout: book
title: scale
permalink: /learn/intrinsics/SCALE
---
## __Name__

__scale__(3) - \[MODEL\_COMPONENTS\] Scale a real value
(GFDL)

## __Syntax__

result = __scale__(x, i)

## __Description__

__scale__(x,i) returns x \* __radix__(x)\*\*i.

## __Arguments__

  - __X__
    The type of the argument shall be a _real_.

  - __I__
    The type of the argument shall be a _integer_.

## __Returns__

The return value is of the same type and kind as X. Its value is x \*
__radix__(x)\*\*i.

## __Examples__

Sample program:

```
    program demo_scale
    implicit none
      real :: x = 178.1387e-4
      integer :: i = 5
      print *, scale(x,i), x*radix(x)**i
    end program demo_scale
```

Results:

```
    0.570043862      0.570043862
```

## __Standard__

Fortran 95 and later

## __See Also__

__radix__(3)
