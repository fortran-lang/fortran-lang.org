---
layout: book
title: log
permalink: /learn/intrinsics/LOG
---
## __Name__

__log__(3) - \[MATHEMATICS\] Logarithm function
(GFDL)

## __Syntax__
```fortran
result = log(x)
```
## __Description__

__log(x)__ computes the natural logarithm of __x__, i.e. the logarithm to
the base "e".

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as __x__. If __x__ is _complex_, the imaginary part OMEGA is in the range

__-PI__ \< OMEGA \<= PI.

## __Examples__

Sample program:

```fortran
program demo_log
implicit none
  real(kind(0.0d0)) :: x = 2.71828182845904518d0
  complex :: z = (1.0, 2.0)
  write(*,*)x, log(x)    ! will yield (approximately) 1
  write(*,*)z, log(z)
end program demo_log
```
  Results:
```text
      2.7182818284590451        1.0000000000000000     
   (1.00000000,2.00000000) (0.804718971,1.10714877)
```
## __Standard__

FORTRAN 77 and later
