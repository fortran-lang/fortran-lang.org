---
layout: book
title: abs
permalink: /learn/intrinsics/ABS
---
## __Name__

__abs__(3) - \[NUMERIC\] Absolute value

## __Syntax__

```fortran
  result = abs(x)
   TYPE(kind=KIND) elemental function abs(a)
   TYPE(kind=KIND),intent(in) :: a
```
where TYPE may be _real_, _integer_, or _complex_
and KIND may be any supported KIND for the
associated TYPE.

## __Description__

__abs(a)__ computes the absolute value of numeric argument __a__.

In mathematics, the absolute value or modulus of a real number __x__,
denoted __|x|__, is the magnitude of __x__ without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by __abs(3)__ when dealing with _complex_
values (_see below_).

## __Arguments__

  - __A__
    the type of the argument shall be an _integer_, _real_, or _complex_
    scalar or array.

## __Returns__

If __A__ is of type _integer_ or _real_, the value of the result is __|A|__ and of
the same type and kind as the input argument.

(Take particular note) if __A__ is _complex_ with value __(X, Y)__, the result is
a _real_ equal to a processor-dependent approximation to
__sqrt(X\*\*2 + Y\*\*2)__
computed without undue overflow or underflow.

## __Examples__

Sample program:

```fortran
program demo_abs
implicit none
integer           :: i = -1
real              :: x = -1.0
complex           :: z = (-3.0,-4.0)
doubleprecision   :: rr = -45.78d+00
character(len=*),parameter :: &
 frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
 frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'
integer,parameter :: dp=kind(0.0d0)

    write(*, frmt)  'integer         ',  i, abs(i)
    write(*, frmt)  'real            ',  x, abs(x)
    write(*, frmt)  'doubleprecision ', rr, abs(rr)
    write(*, frmtc) 'complex         ',  z, abs(z)
    !
    !
    write(*, *)
    write(*, *) 'abs is elemental: ', abs([20,  0,  -1,  -3,  100])
    write(*, *)
    write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
    write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
    write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

    write(*, *)
    write(*, *) 'distance of <XX,YY> from zero is', distance(30.0_dp,40.0_dp)

    contains

    real(kind=dp) elemental function distance(x,y)
    real(kind=dp),intent(in) :: x,y
       ! dusty corners:
       ! note that KIND=DP is NOT optional if the desired result is KIND=dp.
       ! See cmplx(3).
       distance=abs( cmplx(x,y,kind=dp) )
    end function distance
end program demo_abs
```
Results:

```
 integer          In: -1                           Out: 1
 real             In: -1.000000                    Out: 1.000000
 doubleprecision  In: -45.78000000000000           Out: 45.78000000000000
 complex          In: (-3.000000,-4.000000)        Out: 5.000000

 abs is elemental: 20 0 1 3 100

 abs range test :   2147483647  2147483647
 abs range test :   3.4028235E+38  3.4028235E+38
 abs range test :   1.1754944E-38  1.1754944E-38

 distance of <XX,YY> from zero is   50.0000000000000
```

## __Standard__

FORTRAN 77 and later

##### fortran-lang intrinsic descriptions
