---
layout: book
title: erfc
permalink: /learn/intrinsics/ERFC
---
#### NAME

__erfc__(3f) - \[MATHEMATICS\] Complementary error function
(GFDL)

#### SYNTAX

result = __erfc__(x)

#### DESCRIPTION

__erfc__(x) computes the complementary error function of X, defined as
$$ 1 - \\text{erf}(x) = 1 - \\frac{2}{\\sqrt{\\pi}} \\int\_0\*\*x
e\*\*{__-t__\*\*2} dt. $$

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of type REAL and of the same kind as X. It lies in
the range

> 0 \<= __ERFC__(X) \<= 2.

#### EXAMPLE

Sample program:

```
   program demo_erfc
   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.17_real64
     x = erfc(x)
   end program demo_erfc
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental function
