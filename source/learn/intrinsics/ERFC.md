---
layout: book
title: erfc
permalink: /learn/intrinsics/ERFC
---
## __Name__

__erfc__(3) - \[MATHEMATICS\] Complementary error function


## __Syntax__
```fortran
result = erfc(x)

   elemental function erfc(x)
   real(kind=KIND) :: erfc
   real(kind=KIND),intent(in) :: x
```
## __Description__

__erfc__(x) computes the complementary error function of __x__.  Simpy put
this is equivalent to __1 - erf(x)__, but __erfc__ is provided because
of the extreme loss of relative accuracy if __erf(x)__ is called for
large __x__ and the result is subtracted from __1__.

__erfc(x)__ is defined as

<!--
$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt.
$$
-->

$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_x^{\infty} e^{-t^2} dt.
$$

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
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erfc(x)
end program demo_erfc
```
  Results:
```text
     0.17000000000000001       0.81000753879819121     
```
## __Standard__

Fortran 2008 and later

## See also
[__erf__(3)](ERF)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

###### fortran-lang intrinsic descriptions license: MIT) @urbanjost
