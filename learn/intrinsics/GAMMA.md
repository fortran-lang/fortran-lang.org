---
layout: book
title: gamma
permalink: /learn/intrinsics/GAMMA
---
## __Name__

__gamma__(3) - \[MATHEMATICS\] Gamma function
(GFDL)

## __Description__

__gamma__(x) computes Gamma of X. For positive, integer values of X the
Gamma function simplifies to the factorial function
__Gamma__(x)=(x-1)\!.

$$ \\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t $$

## __Syntax__

x = __gamma__(x)

## __Arguments__

  - __X__
    Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as X.

## __Examples__

Sample program:

```fortran
   program demo_gamma
   implicit none
     real :: x = 1.0
     x = gamma(x) ! returns 1.0
   end program demo_gamma
```

## __Standard__

Fortran 2008 and later

## __See Also__

Logarithm of the Gamma function: __\[\[log\_gamma__(3)
