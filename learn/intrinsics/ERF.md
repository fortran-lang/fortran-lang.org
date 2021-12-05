---
layout: book
title: erf
permalink: /learn/intrinsics/ERF
---
## __Name__

__erf__(3) - \[MATHEMATICS\] Error function
(GFDL)

## __Description__

__erf__(x) computes the error function of X, defined as $$
\\text{erf}(x) = \\frac{2}{\\sqrt{\\pi}} \\int\_0\*\*x
e\*\*{__-t__\*\*2} dt. $$

## __Syntax__

result = __erf__(x)

## __Arguments__

  - __X__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_, of the same kind as X and lies in the
range __-1__ \<= __erf__(x) \<= 1 .

## __Examples__

Sample program:

```fortran
program demo_erf
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    x = erf(x)
end program demo_erf
```

## __Standard__

Fortran 2008 and later
