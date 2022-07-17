---
layout: book
title: log10
permalink: /learn/intrinsics/LOG10
---
# LOG10
## __Name__

__log10__(3) - \[MATHEMATICS\] Base 10 logarithm function

## __Syntax__
```fortran
result = log10(x)
 
   real(kind=KIND) elemental function log10(x)
   real(kind=KIND),intent(in) :: x
```
## __Description__

__log10(x)__ computes the base 10 logarithm of __x__. This
is generally called the "common logarithm".

## __Arguments__

  - __x__
    : A _real_ value > 0 to take the log of.

## __Returns__

The return value is of type _real_ . The kind type parameter is
the same as __x__.

## __Examples__

Sample program:

```fortran
program demo_log10
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 10.0_real64

   x = log10(x)
   write(*,'(*(g0))')'log10(',x,') is ',log10(x)

   ! elemental
   write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &
                     & 100000.0, 1000000.0, 10000000.0])

end program demo_log10
```
  Results:
```text
   log10(1.0000000000000000) is 0.0000000000000000
      0.00000000       1.00000000       2.00000000       3.00000000  
      4.00000000       5.00000000       6.00000000       7.00000000    
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
