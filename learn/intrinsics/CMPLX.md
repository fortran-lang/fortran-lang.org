---
layout: book
title: cmplx
permalink: /learn/intrinsics/CMPLX
---
### NAME

__cmplx__(3f) - \[NUMERIC:TYPE\] Complex conversion function
(GFDL)

### SYNTAX

result = __cmplx__(x \[, y \[, kind\]\])

### DESCRIPTION

To convert numeric variables to complex, use the CMPLX function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the CMPLX function.

__CMPLX__(X \[, Y \[, KIND\]\]) returns a complex number where X is
converted to the real component. If X is complex then Y must not be
present. If Y is present it is converted to the imaginary component. If
Y is not present then the imaginary component is set to 0.0.

### CMPLX AND DOUBLE PRECISION

The Fortran 90 language defines __CMPLX__() as always returning a result
that is type __COMPLEX__(KIND=KIND(0.0)).

This means \`__CMPLX__(D1,D2)', where \`D1' and \`D2' are
DOUBLEPRECISION, is treated as:

```
      CMPLX(SNGL(D1), SNGL(D2))
```

DOUBLEPRECISION complex numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
DOUBLEPRECISION arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the __CMPLX__() intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default REAL values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just REAL
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

```
      designator % RE
      or
      designator % IM.
```

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is REAL, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```
       impedance%re           !-- Same value as REAL(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of X to zero
```

### ARGUMENTS

  - __X__
    The type may be INTEGER, REAL, or COMPLEX.

  - __Y__
    (Optional; only allowed if X is not COMPLEX.). May be INTEGER or
    REAL.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of COMPLEX type, with a kind equal to KIND if it is
specified. If KIND is not specified, the result is of the default
COMPLEX kind, regardless of the kinds of X and Y.

### EXAMPLE

Sample program:

```
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
      ! The following is intuitive and works without calling cmplx(3f)
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

### SEE ALSO

  - __aimag__(3f) - Imaginary part of complex number

  - __cmplx__(3f) - Complex conversion function

  - __conjg__(3f) - Complex conjugate function

  - __real__(3f) - Convert to real type

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function
