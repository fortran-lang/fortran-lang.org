## abs
### __Name__

__abs__(3) - \[NUMERIC\] Absolute value

### __Syntax__
```fortran
  result = abs(a)

   TYPE(kind=KIND) elemental function abs(a)

   TYPE(kind=KIND),intent(in) :: a
```
where the TYPE and KIND is determined by the type and type attributes
of __a__, which may be any _real_, _integer_, or _complex_ value.

If the type of __a__ is _cmplx_ the type returned will be _real_ with
the same kind as the _real_ part of the input value.

Otherwise the returned type will be the same type as __a__.

### __Description__

__abs(a)__ computes the absolute value of numeric argument __a__.

In mathematics, the absolute value or modulus of a real number __x__,
denoted __|x|__, is the magnitude of __x__ without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by __abs__(3) when dealing with
_complex_ values (_see below_).

### __Arguments__

  - __a__
    : the type of the argument shall be an _integer_, _real_, or _complex_
    scalar or array.

### __Returns__

If __a__ is of type _integer_ or _real_, the value of the result is
__|a|__ and of the same type and kind as the input argument.

(Take particular note) if __a__ is _complex_ with value __(x, y)__,
the result is a _real_ equal to a processor-dependent approximation to
__sqrt(x\*\*2 + y\*\*2)__
computed without undue overflow or underflow.

### __Examples__

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
integer,parameter :: sp=kind(0.0)

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

    write(*, *) 'returned real kind:', cmplx(30.0_dp,40.0_dp,kind=dp), &
                                  kind(cmplx(30.0_dp,40.0_dp,kind=dp))
    write(*, *) 'returned real kind:', cmplx(30.0_dp,40.0_dp),&
                                  kind(cmplx(30.0_dp,40.0_dp))
    write(*, *) 'returned real kind:', cmplx(30.0_sp,40.0_sp),&
                                  kind(cmplx(30.0_sp,40.0_sp))

    write(*, *)
    write(*, *) 'distance of <XX,YY> from zero is', &
               & distance(30.0_dp,40.0_dp)

    contains

    real(kind=dp) elemental function distance(x,y)
    real(kind=dp),intent(in) :: x,y
       ! dusty corners:
       ! note that KIND=DP is NOT optional 
       ! if the desired result is KIND=dp.
       ! See cmplx(3).
       distance=abs( cmplx(x,y,kind=dp) )
    end function distance
end program demo_abs
```
  Results:
```text
    integer          In: -1                        Out: 1
    real             In: -1.00000000               Out: 1.00000000
    doubleprecision  In: -45.780000000000001       Out: 45.780000000000001
    complex          In: (-3.00000000,-4.00000000) Out: 5.00000000
   
    abs is elemental:     20     0     1     3   100
   
    abs range test :   2147483647  2147483647
    abs range test :    3.40282347E+38   3.40282347E+38
    abs range test :    1.17549435E-38   1.17549435E-38
    returned real kind: (30.000000000000000,40.000000000000000) 8
    returned real kind: (30.0000000,40.0000000) 4
    returned real kind: (30.0000000,40.0000000) 4
   
    distance of <XX,YY> from zero is   50.000000000000000     
```
### __Standard__

FORTRAN 77 and later

####### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
