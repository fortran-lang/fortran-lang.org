---
layout: book
title: cmplx
permalink: /learn/intrinsics/CMPLX
---
## __Name__

__cmplx__(3) - \[NUMERIC:TYPE\] Complex conversion function
(GFDL)

## __Syntax__
```fortran
result = cmplx(x, y, kind)
```

## __Description__

To convert numeric variables to complex, use the __cmplx__(3) function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the __cmplx__(3) function.

__cmplx(x \[, y \[, kind\]\])__ returns a complex number where __x__ is
converted to the _real_ component. If __x__ is _complex_ then __y__ must not be
present. If __y__ is present it is converted to the imaginary component. If
__y__ is not present then the imaginary component is set to __0.0__.

## __cmplx__(3) and double precision__

The Fortran 90 language defines __cmplx__(3) as always returning a result
that is type ___complex___(kind=KIND(0.0)).

This means \`__cmplx(d1,d2)__', where __\`d1'__ and __\`d2'__ are
_doubleprecision_, is treated as:
fortran
```
      cmplx(sngl(d1), sngl(d2))
```

_doubleprecision complex_ numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
_doubleprecision_ arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the __cmplx__(3) intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default _real_ values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just _real_
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```

F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity
can be accessed independently with a component-like syntax in f2018:

A complex-part-designator is

``fortran
      designator % RE
      or
      designator % IM.
```

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is _real_, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
```

## __Arguments__

  - __x__
    The type may be _integer_, _real_, or _complex_.

  - __y__
    (Optional; only allowed if __x__ is not _complex_.). May be _integer_ or
    _real_.

  - __kind__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of _complex_ type, with a kind equal to __kind__ if it is
specified. If __kind__ is not specified, the result is of the default
_complex_ kind, regardless of the kinds of __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
complex          :: z4
complex(kind=dp) :: z8
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
   print *, 'Z8=',z8
   ! NOTE:
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )
   print *, 'Z8 defined with constants=',z8
end program demo_aimag
```

Typical Results:

```
    Z4= (1.23456788,1.23456788)
    Z8= (1.2345678901234567,1.2345678901234567)
    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)
```

## __See Also__

  - [__aimag__(3)](AIMAG) - Imaginary part of complex number

  - [__cmplx__(3)](CMPLX) - Complex conversion function

  - [__conjg__(3)](CONJG) - Complex conjugate function

  - [__real__(3)](REAL) - Convert to real type

## __Standard__

FORTRAN 77 and later
