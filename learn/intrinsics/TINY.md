---
layout: book
title: tiny
permalink: /learn/intrinsics/TINY
---
## __Name__

__tiny__(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind
(GFDL)

## __Syntax__
```fortran
result = tiny(x)
   real(kind=KIND) function(x)
   real(kind=KIND) :: x
```
  where KIND may be any kind supported by type _real_

## __Description__

__tiny(x)__ returns the smallest positive (non zero) number of the type
and kind of __x_.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The smallest positive value for the _real_ type of the specified kind.

The return value is of the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tiny
implicit none
   print *, 'default real is from',tiny(0.0) ,'to',huge(0.0)
   print *, 'doubleprecision is from ',tiny(0.0d0),'to',huge(0.0d0)
end program demo_tiny
```
Results:
```text
 default real is from 1.17549435E-38 to 3.40282347E+38
 doubleprecision is from 2.2250738585072014E-308 to 1.7976931348623157E+308
```
## __Standard__

Fortran 95 and later
## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
[fraction(3)](FRACTION),
[huge(3)](HUGE),
[maxexponent(3)](MAXEXPONENT),
[minexponent(3)](MINEXPONENT),
[nearest(3)](NEAREST),
[precision(3)](PRECISION),
[radix(3)](RADIX),
[range(3)](RANGE),
[rrspacing(3)](RRSPACING),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING)
