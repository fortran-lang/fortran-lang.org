---
layout: book
title: abs
permalink: /learn/intrinsics/ABS
---
### NAME

**abs**(3f) - \[NUMERIC\] Absolute value

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

**abs**(A) computes the absolute value of numeric argument A.

### ARGUMENTS

  - **A**
    the type of the argument shall be an INTEGER, REAL, or COMPLEX
    scalar or array with **INTENT**(IN).

### RETURN VALUE

If A is of type INTEGER or REAL, the value of the result is |A| and of
the same type and kind as the input argument.

if A is COMPLEX with value (X, Y), the result is a REAL equal to a
processor-dependent approximation to **SQRT**(X\*\*2 + Y\*\*2) computed
without undue overflow or underflow.

### EXAMPLE

Sample program:

```fortran
program demo_abs
integer :: i = -1, iout
real :: x = -1.e0, xout, zout
complex :: z = (-3.e0,4.e0)
doubleprecision :: r8 = 45.78D+00, dout
   write(*,*)'INPUTS:',i,x,z,r8
   iout = abs(i)
   xout = abs(x)
   zout = abs(z)
   dout = abs(r8)
   write(*,*)'OUTPUTS:',iout,xout,zout,dout
   write ( *, '(a,f12.4,12x,f12.4)' ) ' Double precision  ', -r8, abs(r8)
   ! COMPLEX
   ! 3 - 4 -5 right triangle test :
   write(*,*)'The abs() of (3.0,4.0) should be 5.0',abs((3.0,4.0))
   ! ELEMENTAL
   write(*,*)'abs is ELEMENTAL: ',abs([-10, 20, 0, -1, -3, 100])
end program demo_abs
```

Results:

```
    INPUTS:  -1  -1.00000000 (-3.00000000,4.00000000)   45.780000000000001
    OUTPUTS:  1   1.00000000 5.00000000                 45.780000000000001
    Double precision -45.7800 45.7800
    The abs() of (3.0,4.0) should be 5.0   5.00000000
    abs is ELEMENTAL: 10 20 0 1 3 100
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

#### @urbanjost
