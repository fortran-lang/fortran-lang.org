---
layout: book
title: acosh
permalink: /learn/intrinsics/ACOSH
---
-------------------------------------------------------------------------------
## __Name__

__acosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic cosine function

## __Syntax__

```fortran
  result = acosh(x)
   TYPE(kind=KIND),elemental :: acosh
   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

## __Description__

__acosh(X)__ computes the inverse hyperbolic cosine of __X__ in radians.

## __Arguments__

  - __X__
    the type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __X__.

If __X__ is _complex_, the imaginary part of the result is in radians and
lies between

> __0 \<= aimag(acosh(x)) \<= PI__

## __Examples__

Sample program:

```fortran
program demo_acosh
implicit none
real(8), dimension(3) :: x = [ 1.0, 2.0, 3.0 ]
   write (*,*) acosh(x)
end program demo_acosh
```

## __Standard__

Fortran 2008 and later

## __See Also__

Inverse function: [__cosh__(3)](COSH)
