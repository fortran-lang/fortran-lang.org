---
layout: book
title: sinh
permalink: /learn/intrinsics/SINH
---
# SINH
## __Name__

__sinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function

## __Syntax__
```fortran
result = sinh(x)

    elemental TYPE(kind=KIND) function sinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value 
and TYPE may be _real_ or _complex_

## __Description__

__sinh(x)__ computes the hyperbolic sine of __x__.

The hyperbolic sine of x is defined mathematically as:

   __sinh(x) = (exp(x) - exp(-x)) / 2.0__

If __x__ is of type _complex_ its imaginary part is regarded as a value
in radians.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_sinh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = - 1.0_real64
real(kind=real64) :: nan, inf
character(len=20) :: line

   print *, sinh(x)
   print *, (exp(x)-exp(-x))/2.0

   ! sinh(3) is elemental and can handle an array
   print *, sinh([x,2.0*x,x/3.0])

   ! a NaN input returns NaN
   line='NAN'
   read(line,*) nan
   print *, sinh(nan)

   ! a Inf input returns Inf
   line='Infinity'
   read(line,*) inf
   print *, sinh(inf)

   ! an overflow returns Inf
   x=huge(0.0d0)
   print *, sinh(x)

end program demo_sinh
```
Results:
```text
  -1.1752011936438014     
  -1.1752011936438014     
  -1.1752011936438014       -3.6268604078470190      -0.33954055725615012     
                       NaN
                  Infinity
                  Infinity
```
## __Standard__

Fortran 95 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__asinh__(3)](ASINH)

###### fortran-lang intrinsic descriptions
