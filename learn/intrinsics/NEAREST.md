---
layout: book
title: nearest
permalink: /learn/intrinsics/NEAREST
---
#### NAME

__nearest__(3f) - \[MODEL\_COMPONENTS\] Nearest representable number
(GFDL)

#### SYNTAX

result = __nearest__(x, s)

#### DESCRIPTION

__nearest__(x, s) returns the processor-representable number nearest to
X in the direction indicated by the sign of S.

#### ARGUMENTS

  - __X__
    Shall be of type REAL.

  - __S__
    Shall be of type REAL and not equal to zero.

#### RETURN VALUE

The return value is of the same type as X. If S is positive, NEAREST
returns the processor-representable number greater than X and nearest to
it. If S is negative, NEAREST returns the processor-representable number
smaller than X and nearest to it.

#### EXAMPLE

Sample program:

```
   program demo_nearest
   implicit none
     real :: x, y
     x = nearest(42.0, 1.0)
     y = nearest(42.0, -1.0)
     write (*,"(3(g20.15))") x, y, x - y
   end program demo_nearest
```

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function
