---
layout: book
title: tan
permalink: /learn/intrinsics/TAN
---
## __Name__

__tan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function
(GFDL)

## __Syntax__

result = __tan(x)__

## __Description__

__tan(x)__ computes the tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 0.165_real64
     x = tan(x)
end program demo_tan
```

## __Standard__

FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.

## __See Also__

[__atan__(3)](ATAN),
[__cos__(3)](COS),
[__sin__(3)](SIN)
