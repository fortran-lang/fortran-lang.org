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
denoted |x|, is the positive magnitude of x without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by abs(1) when dealing with COMPLEX
values (see below).

### ARGUMENTS

  - __A__
    the type of the argument shall be an INTEGER, REAL, or COMPLEX
    scalar or array.

### RETURN VALUE


If A is of type INTEGER or REAL, the value of the result is |A| and of
the same type and kind as the input argument.

(Take particular note) if A is COMPLEX with value (X, Y), the result is
a REAL equal to a processor-dependent approximation to __SQRT__(X\*\*2 +
Y\*\*2) computed without undue overflow or underflow.

### EXAMPLE

Sample program:

```fortran
program demo_abs
integer         :: i = -1 
real            :: x = -1.e0 
complex         :: z = (-3.e0,-4.e0)
doubleprecision :: r8 = -45.78D+00 
character(len=*),parameter :: &
 frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
 frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'

    write(*, frmt)  'integer         ',  i, abs(i)
    write(*, frmt)  'real            ',  x, abs(x)
    write(*, frmt)  'doubleprecision ', r8, abs(r8)
    write(*, frmtc) 'complex         ',  z, abs(z)
    write(*, *)
    write(*, *)'abs is elemental: ', abs([20,  0,  -1,  -3,  100])
    write(*, *)
    write(*, *)'abs range test : ', abs(huge(0)), abs(-huge(0))
    write(*, *)'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
    write(*, *)'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

end program demo_abs
```

Results:

```
 integer          In: -1                        Out: 1
 real             In: -1.00000000               Out: 1.00000000
 doubleprecision  In: -45.780000000000001       Out: 45.780000000000001
 complex          In: (-3.00000000,-4.00000000) Out: 5.00000000

 abs is elemental: 20 0 1 3 100

 abs range test :  2147483647  2147483647
 abs range test :  3.40282347E+38   3.40282347E+38
 abs range test :  1.17549435E-38   1.17549435E-38
```

### STANDARD

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
</details>
