---
layout: book
title: log_gamma
permalink: /learn/intrinsics/LOG_GAMMA
---
## __Name__

__log\_gamma__(3) - \[MATHEMATICS\] Logarithm of the Gamma function
(GFDL)

## __Syntax__

x = __log\_gamma__(x)

## __Description__

__log\_gamma__(x) computes the natural logarithm of the absolute value
of the \[\[Gamma function\]\].

## __Arguments__

  - __X__
    Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as X.

## __Examples__

Sample program:

```fortran
   program demo_log_gamma
   implicit none
     real :: x = 1.0
     x = log_gamma(x) ! returns 0.0
   end program demo_log_gamma
```

## __Standard__

Fortran 2008 and later

## __See Also__

Gamma function: __gamma__(3)
