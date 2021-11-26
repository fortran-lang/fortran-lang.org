---
layout: book
title: erfc_scaled
permalink: /learn/intrinsics/ERFC_SCALED
---
## __Name__

__erfc\_scaled__(3) - \[MATHEMATICS\] Error function
(GFDL)

## __Description__

__erfc\_scaled__(x) computes the exponentially-scaled complementary
error function of X:

$$ e\*\*{x\*\*2} \\frac{2}{\\sqrt{\\pi}} \\int\_{x}\*\*{\\infty}
e\*\*{__-t__\*\*2} dt. $$

## __Syntax__

result = __erfc\_scaled__(x)

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as X.

## __Examples__

Sample program:

```
   program demo_erfc_scaled
   implicit none
   real(kind(0.0d0)) :: x = 0.17d0
     x = erfc_scaled(x)
     print *, x ! prints approx. 0.83375830214998126
   end program demo_erfc_scaled
```

## __Standard__

Fortran 2008 and later
