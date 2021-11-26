---
layout: book
title: abs
permalink: /learn/intrinsics/ABS
---
### NAME

__abs__(3f) - \[NUMERIC\] Absolute value

### SYNTAX
```fortran
   result = ABS(A)

    TYPE(kind=KIND),elemental  :: abs
    TYPE(kind=KIND),intent(in) :: a
    TYPE(kind=KIND)            :: result

    where TYPE may be REAL, INTEGER, or COMPLEX
    and KIND may be any supported KIND for the
    associated TYPE.
```
### DESCRIPTION

__abs__(A) computes the absolute value of numeric argument A.

In mathematics, the absolute value or modulus of a real number x,
denoted |x|, is the magnitude of x without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by __abs(3f)__ when dealing with COMPLEX
values (see below).

### ARGUMENTS

  - __A__
    the type of the argument shall be an INTEGER, REAL, or COMPLEX
    scalar or array.

### RETURN VALUE

If __A__ is of type INTEGER or REAL, the value of the result is __|A|__ and of
the same type and kind as the input argument.

(Take particular note) if __A__ is COMPLEX with value __(X, Y)__, the result is
a REAL equal to a processor-dependent approximation to 
__SQRT(X\*\*2 + Y\*\*2)__ 
computed without undue overflow or underflow.

### EXAMPLE

Sample program:

```fortran
program demo_abs
implicit none
integer,parameter :: dp=kind(0.0d0)
integer           :: i = -1 
real              :: x = -1.0 
complex           :: z = (-3.0,-4.0)
doubleprecision   :: rr = -45.78d+00
real(kind=dp)     :: xx, yy
character(len=*),parameter :: &
 frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
 frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'

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
    !
    ! dusty corners: 
    ! note here that KIND=DP is NOT optional if the desired result is dp.
    ! See cmplx(3f).
    write(*, *)
    XX=-30.0_dp
    YY=40.0_dp
    write(*, *) 'distance of <XX,YY> from zero is',   &
    & abs( cmplx(XX,YY,kind=dp) )

end program demo_abs
```
Results:

```
 integer          In: -1                           Out: 1
 real             In: -1.000000                    Out: 1.000000
 doubleprecision  In: -45.78000000000000           Out: 45.78000000000000
 complex          In: (-3.000000,-4.000000)        Out: 5.000000
    
 abs is elemental:           20           0           1           3         100
    
 abs range test :   2147483647  2147483647
 abs range test :   3.4028235E+38  3.4028235E+38
 abs range test :   1.1754944E-38  1.1754944E-38

 distance of <XX,YY> from zero is   50.0000000000000     
```

### STANDARD

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
