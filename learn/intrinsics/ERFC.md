---
layout: book
title: erfc
permalink: /learn/intrinsics/ERFC
---
## __Name__

__erfc__(3) - \[MATHEMATICS\] Complementary error function
(GFDL)

## __Syntax__
```fortran
result = erfc(x)
```

## __Description__

__erfc__(x) computes the complementary error function of __x__, defined as
$$ 1 - \\text{erf}(x) = 1 - \\frac{2}{\\sqrt{\\pi}} \\int\_0\*\*x
e\*\*{__-t__\*\*2} dt. $$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as __x__. It lies in
the range

> 0 \<= __erfc__(x) \<= 2.

## __Examples__

Sample program:

```fortran
program demo_erfc
use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    x = erfc(x)
end program demo_erfc
```

## __Standard__

Fortran 2008 and later
