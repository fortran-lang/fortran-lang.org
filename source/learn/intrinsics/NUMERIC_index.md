# NUMERIC_index
### Manipulation and properties of numeric values

# ABS

## __Name__

__abs__(3) - \[NUMERIC\] Absolute value

## __Syntax__
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

## __Description__

__abs(a)__ computes the absolute value of numeric argument __a__.

In mathematics, the absolute value or modulus of a real number __x__,
denoted __|x|__, is the magnitude of __x__ without regard to its sign.

The absolute value of a number may be thought of as its distance from
zero, which is the definition used by __abs__(3) when dealing with
_complex_ values (_see below_).

## __Arguments__

  - __a__
    : the type of the argument shall be an _integer_, _real_, or _complex_
    scalar or array.

## __Returns__

If __a__ is of type _integer_ or _real_, the value of the result is
__|a|__ and of the same type and kind as the input argument.

(Take particular note) if __a__ is _complex_ with value __(x, y)__,
the result is a _real_ equal to a processor-dependent approximation to
__sqrt(x\*\*2 + y\*\*2)__
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
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

# AINT
## __Name__

__aint__(3) - \[NUMERIC\] Truncate to a whole number


## __Syntax__
```fortran
result = aint(x)

   real(kind=kind(x)),elemental  :: aint

   real(kind=kind(x)),intent(in) :: x
```
or
```fortran
result = aint(x, KIND)

   real(kind=KIND),elemental     :: aint

   integer,intent(in),optional   :: KIND
   real(kind=kind(x)),intent(in) :: x
```
## __Description__

__aint(x, kind)__ truncates its argument to a whole number.

## __Arguments__

  - __x__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the
    kind parameter of the result.

## __Returns__

The return value is of type _real_ with the kind type parameter of
the argument if the optional __kind__ is absent; otherwise, the kind
type parameter will be given by __kind__. If the magnitude of __x__
is less than one, __aint(x)__ returns zero. If the magnitude is equal
to or greater than one then it returns the largest whole number that
does not exceed its magnitude. The sign is the same as the sign of __x__.

## __Examples__

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real32, real64
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 4.3210_real32
   x8 = 4.3210_real64
   print *, aint(x4), aint(x8)
   print *
   ! elemental
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_aint
```
  Results:
```text
     4.00000000       4.0000000000000000     
   
    -2.00000000      -2.00000000      -2.00000000      -2.00000000
    -1.00000000      -1.00000000      -0.00000000       0.00000000
     0.00000000       1.00000000       1.00000000       2.00000000
     2.00000000       2.00000000       2.00000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# ANINT
## __Name__

__anint__(3) - \[NUMERIC\] Nearest whole number


## __Syntax__
```fortran
result = anint(a, kind)
```
## __Description__

__anint(a \[, kind\])__ rounds its argument to the nearest whole number.

## __Arguments__

  - __a__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type real with the kind type parameter of the
argument if the optional __kind__ is absent; otherwise, the kind type
parameter will be given by __kind__. If __a__ is greater than zero, __anint(a)__
returns __aint(a + 0.5)__. If __a__ is less than or equal to zero then it
returns __aint(a - 0.5)__.

## __Examples__

Sample program:

```fortran
program demo_anint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 1.234E0_real32
   x8 = 4.321_real64
   print *, anint(x4), dnint(x8)
   x8 = anint(x4,kind=real64)
   print *, x8
   print *
   ! elemental
   print *,anint([ &
    & -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0, &
    & +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_anint
```
  Results:
```text
    1.00000000       4.0000000000000000     
    1.0000000000000000     
  
   -3.00000000      -3.00000000      -2.00000000      -2.00000000
   -2.00000000      -1.00000000      -1.00000000       0.00000000
    1.00000000       1.00000000       2.00000000       2.00000000
    2.00000000       3.00000000       3.00000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__aint__(3)](AINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# CEILING
## __Name__

__ceiling__(3) - \[NUMERIC\] Integer ceiling function


## __Syntax__
```fortran
result = ceiling(a, kind)

   integer(kind=KIND) elemental function ceiling(a,kind) 
   real(kind=ANY),intent(in)   :: a
   integer,intent(in),optional :: kind
```
## __Description__

__ceiling(a)__ returns the least integer greater than or equal to __a__.

## __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type __integer__(kind) if __kind__ is present and a
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

## __Examples__

Sample program:

```fortran
program demo_ceiling
implicit none
real :: x = 63.29
real :: y = -63.59
   print *, ceiling(x) 
   print *, ceiling(y) 
   ! elemental
   print *,ceiling([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
end program demo_ceiling
```
  Results:
```text
   64
  -63
   -2      -2      -2      -2      -1      -1
    0       0       1       1       2       2
    3       3       3
```
## __Standard__

Fortran 95 and later

## __See Also__

[__floor__(3)](FLOOR),
[__nint__(3)](NINT)


[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND)

###### fortran-lang intrinsic descriptions
# CONJG
## __Name__

__conjg__(3) - \[NUMERIC\] Complex conjugate of a complex value

## __Syntax__
```fortran
z = conjg(z)

   complex(kind=K) elemental function conjg(z)
   complex(kind=K),intent(in) :: z 
```
where __K__ is the kind of the parameter __z__

## __Description__

__conjg(z)__ returns the complex conjugate of the _complex_ value __z__. 

In mathematics, the complex conjugate of a complex_ number is the number
with an equal real part and an imaginary part equal in magnitude but
opposite in sign.  

That is, If __z__ is __(x, y)__ then the result is __(x, -y)__.

For matrices of complex numbers, __conjg(array)__ represents the
element-by-element conjugation of __array__; not the conjugate transpose
of __array__ .

## __Arguments__

  - __z__
    : The type shall be _complex_.

## __Returns__

The return value is of type _complex_.

## __Examples__

Sample program:

```fortran
program demo_conjg
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
complex :: z = (2.0, 3.0)
complex(kind=real64) :: dz = (   &
   &  1.2345678901234567_real64, &
   & -1.2345678901234567_real64)
complex :: arr(3,3)
integer :: i

    print *, z
    z= conjg(z)
    print *, z
    print *

    print *, dz
    dz = conjg(dz)
    print *, dz
    print *

    ! the function is elemental so it can take arrays
    arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]
    arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]
    arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]

    write(*,*)'original'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)
    arr = conjg(arr)
    write(*,*)'conjugate'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)

end program demo_conjg
```
  Results:
```fortran
 (2.000000,3.000000)
 (2.000000,-3.000000)
 
 (1.23456789012346,-1.23456789012346)
 (1.23456789012346,1.23456789012346)
 
 original
(-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )
( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )
( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )

 conjugate
(-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )
( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )
( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DIM
## __Name__

__dim__(3) - \[NUMERIC\] Positive difference


## __Syntax__
```fortran
result = dim(x, y)

    elemental function dim(x, y)
    type(TYPE(kind=KIND))            :: dim
    type(TYPE(kind=KIND)),intent(in) :: x, y
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind for the type.
## __Description__

__dim(x,y)__ returns the difference __x - y__ if the result is positive;
otherwise it returns zero.

## __Arguments__

  - __x__
    : The type shall be _integer_ or _real_

  - __y__
    : The type shall be the same type and kind as __x__.

## __Returns__

The return value is the same type and kind as the input arguments __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x
    i = dim(4, 15)
    x = dim(4.321_real64, 1.111_real64)
    print *, i
    print *, x
    ! elemental
    print *, dim([1,2,3],2)
    print *, dim([1,2,3],[3,2,1])
    print *, dim(-10,[0,-10,-20])
end program demo_dim
```
Results:
```text
              0
      3.21000000000000     
              0           0           1
              0           0           2
              0           0          10
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DPROD
## __Name__

__dprod__(3) - \[NUMERIC\] Double product function


## __Syntax__
```fortran
result = dprod(x, y)
```
## __Description__

__dprod(x,y)__ produces a higher _doubleprecision_ product of default _real_
numbers __x__ and __y__.

The result has a value equal to a processor-dependent approximation to
the product of __x__ and __y__. It is recommended that the processor compute the
product in double precision, rather than in single precision and then
converted to double precision.

  - __x__
    : shall be default real.

  - __y__
    : shall be default real.

The setting of compiler options specifying _real_ size can affect this
function.

## __Arguments__

  - __x__
    : Must be of default _real(kind=kind(0.0))_ type

  - __y__
    : Must have the same type and kind parameters as __x__

## __Returns__

The return value is of type _real(kind=kind(0.0d0))_.

## __Examples__

Sample program:

```fortran
program demo_dprod
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
real(kind=dp) :: dd
   dd = dprod(x,y)
   print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
   ! interesting comparisons
   print *, 52*23
   print *, 52*23/100.0
   print *, 52*23/100.0d0

   !! common extension is to take doubleprecision arguments
   !! and return higher precision
   bigger: block
   doubleprecision :: xx = 5.2d0
   doubleprecision :: yy = 2.3d0
   real(kind=real128) :: ddd
   !ddd = dprod(xx,yy)
   !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
   endblock bigger

end program demo_dprod
```
  Results:
```text
   11.959999313354501 11.9599991 4 8 8
        1196
   11.9600000    
   11.960000000000001     
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# FLOOR
## __Name__

__floor__(3) - \[NUMERIC\] function to return largest integral value not greater than argument

## __Syntax__
```fortran
result = floor(a, KIND)

    elemental function floor(a,KIND)
    integer(kind=KIND) :: floor
    real(kind=kind(a)),intent(in) :: a
    integer(kind=IKIND),intent(in),optional :: KIND
```
    where __KIND__ is any valid value for type _integer_.
## __Description__

__floor(a)__ returns the greatest integer less than or equal to __a__.
That is, it picks the whole number at or to the left of the value on
the scale __-huge(int(a,kind=KIND))-1__ to __huge(int(a),kind=KIND)__.

## __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : (Optional) A scalar _integer_ constant initialization expression
    indicating the kind parameter of the result.

## __Returns__

The return value is of type _integer(kind)_ if __kind__ is present and of
default-kind _integer_ otherwise. 

The result is undefined if it cannot be represented in the specified
integer type.

## __Examples__

Sample program:

```fortran
program demo_floor
implicit none
real :: x = 63.29
real :: y = -63.59
    print *, x, floor(x) 
    print *, y, floor(y) 
   ! elemental
   print *,floor([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

   ! note even a small deviation from the whole number changes the result
   print *,      [2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)]
   print *,floor([2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)])

   ! A=Nan, Infinity or  <huge(0_KIND)-1 < A > huge(0_KIND) is undefined
end program demo_floor
```
Results:
```text
      63.29000              63
     -63.59000             -64
             -3          -3          -3          -2          -2          -1
             -1           0           0           1           1           2
              2           2           2
      2.000000       2.000000       2.000000    
              2           1           1
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ceiling__(3)](CEILING),
[__nint__(3)](NINT)


[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MAX
## __Name__

__max__(3) - \[NUMERIC\] Maximum value of an argument list

## __Syntax__
```fortran
result = max(a1, a2, a3, ...)
```
## __Description__

Returns the argument with the largest (most positive) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2,a3,...__
    : An expression of the same type and kind as __a1__.

## __Returns__

The return value corresponds to the maximum value among the arguments,
and has the same type and kind as the first argument.

The function is both elemental and allows for an arbitrary number of
arguments. This means if some elements are scalar and some are arrays
that all the arrays must be of the same size, and the returned value
will be an array that is the result as if multiple calls were made with
all scalar values with a single element of each array used in each call.
If called with all arrays the returned array is the same as if multiple
calls were made with __max(arr1(1),arr2(1), ...)__ to
__max(arr1(N),arr2(N))__.

## __Examples__

Sample program

```fortran
program demo_max
implicit none
real :: arr1(4)= [10.0,11.0,30.0,-100.0]
real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

  !! this is simple enough because it is not being called elementally
  !! because all arguments are scalar
  !!

  write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)

  !!
  !! this is all max(3) could do before it became an elemental
  !! function and is the most intuitive
  !! except that it can take an arbitrary number of options,
  !! which is not common in Fortran without
  !! declaring a lot of optional parameters.
  !!
  !! That is it unless you want to use the elemental features of max(3)!

  !! Error: Intrinsic    max    at (1) must have at least two arguments
  !!write(*,*)max(arr1)
  !! This does not work because it is like trying to return
  !! [(max(arr1(i)),i=1,size(arr1))]
  !! so it is trying to take the max of a single value.
  !! To find the largest element of an array
  !! call maxloc(3) or maxval(3).

  !! Error: Different shape for arguments 'a1' and 'a2' for intrinsic
  !! 'max' at (1) on dimension 1 (4 and 5)
  !!write(*,*)max(arr1,arr2)
  !! but this will return an array of
  !! [(max(arr1(N),arr2(N),N=1,size(arr1))]

  write(*,*)max(arr1,arr2(1:4))

  !! so this works only if all the arrays are the same size and
  !! you want an array of the largest Nth elements
  !! from the input arrays.
  !! maybe you wanted to do maxval([arr1,arr2]) or
  !! equivalently max(maxval(arr1),maxval(arr2))
  !! to find the single largest element in both arrays?

  !! compares all scalars to each member of array and
  !! returns array of size arr2

  write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

  !! Error: Different shape for arguments 'a5' and 'a6'
  !! for intrinsic 'max' at (1) on dimension 1 (5 and 4)
  !! write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2,arr1)
  !! as the same reason above when arrays are used
  !! (without scalar values) all the arrays must be the same size

  write(*,*)'scalars and array:',&
  & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)
end program demo_max
```
Results:

```text
    scalars:   30.000000
      20.0000000  21.000000  32.000000 -100.00000
    scalars and array: 30.000000 30.000000 32.000000 30.000000 2200.0000
    scalars and array: 40.000000 40.000000 40.000000 40.000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__maxloc__(3)](MAXLOC),
[__maxval__(3)](MAXVAL),
[__min__(3)](MIN)

###### fortran-lang intrinsic descriptions
# MIN
## __Name__

__min__(3) - \[NUMERIC\] Minimum value of an argument list


## __Syntax__
```fortran
result = min(a1, a2, a3, ... )
```
## __Description__

Returns the argument with the smallest (most negative) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2, a3, \`\`\`__
    : An expression of the same type and kind as __A1__.

## __Returns__

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

## __Examples__

Sample program

```fortran
program demo_min
implicit none
    write(*,*)min(10.0,11.0,30.0,-100.0)
end program demo_min
```

Results:

```
      -100.0000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__max__(3)](MAX),
[__minloc__(3)](MINLOC),
[__minval__(3)](MINVAL)

###### fortran-lang intrinsic descriptions
# MOD
## __Name__

__mod__(3) - \[NUMERIC\] Remainder function


## __Syntax__
```fortran
result = mod(a, p)
```
## __Description__

__mod__(a,p) computes the remainder of the division of __a__ by __p__.

## __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__ and not equal to
    zero.

## __Returns__

The return value is the result of __a - (int(a/p) \* p)__. The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as __a__ and a magnitude less than the magnitude of
__p__.

## __Examples__

Sample program:

```fortran
program demo_mod
implicit none
     print *, mod(17,3)           ! yields 2
     print *, mod(17.5,5.5)       ! yields 1.0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

     print *, mod(-17,3)          ! yields -2
     print *, mod(-17.5,5.5)      ! yields -1.0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

     print *, mod(17,-3)          ! yields 2
     print *, mod(17.5,-5.5)      ! yields 1.0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
end program demo_mod
```
  Results:
```text
              2
      1.00000000    
      1.0000000000000000     
      1.0000000000000000     
             -2
     -1.00000000    
     -1.0000000000000000     
     -1.0000000000000000     
              2
      1.00000000    
      1.0000000000000000     
      1.0000000000000000     
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__modulo__(3)](MODULO)

###### fortran-lang intrinsic descriptions
# MODULO
## __Name__

__modulo__(3) - \[NUMERIC\] Modulo function


## __Syntax__
```fortran
result = modulo(a, p)
```
## __Description__

__modulo(a,p)__ computes the __a__ modulo __p__.

## __Arguments__

  - __a__
    : Shall be a scalar of type _integer_ or _real_.

  - __p__
    : Shall be a scalar of the same type and kind as __a__. It shall not be
      zero.

## __Returns__

The type and kind of the result are those of the arguments.

  - If __a__ and __p__ are of type _integer_: __modulo(a,p)__ has the value of 
    __a - floor (real(a) / real(p)) \* p__.

  - If __a__ and __p__ are of type _real_: __modulo(a,p)__ has the value of 
    __a - floor (a / p) \* p__.

The returned value has the same sign as __p__ and a magnitude less than the
magnitude of __p__.

## __Examples__

Sample program:

```fortran
program demo_modulo
implicit none
     print *, modulo(17,3)        ! yields 2
     print *, modulo(17.5,5.5)    ! yields 1.0

     print *, modulo(-17,3)       ! yields 1
     print *, modulo(-17.5,5.5)   ! yields 4.5

     print *, modulo(17,-3)       ! yields -1
     print *, modulo(17.5,-5.5)   ! yields -4.5
end program demo_modulo
```
  Results:
```text
              2
      1.00000000    
              1
      4.50000000    
             -1
     -4.50000000    
```
## __Standard__

Fortran 95 and later

## __See Also__

[__mod__(3)](MOD)

###### fortran-lang intrinsic descriptions
# SIGN
## __Name__

__sign__(3) - \[NUMERIC\] Sign copying function


## __Syntax__
```fortran
result = sign(a, b)

    elemental function sign(a, b)
    type(TYPE(kind=KIND))            :: sign
    type(TYPE(kind=KIND)),intent(in) :: a, b
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind for the type.
```
## __Description__

__sign__(a,b) returns the value of __a__ with the sign of __b__.


For processors that distinguish between positive and negative zeros  __sign()__ may be used to 
distinguish between __real__ values 0.0 and −0.0. SIGN (1.0, -0.0) will 
return −1.0 when a negative zero is distinguishable.

    29  1 Description. Magnitude of A with the sign of B.
      


## __Arguments__

  - __a__
    : Shall be of type _integer_ or _real_

  - __b__
    : Shall be of the same type and kind as __a__

## __Returns__

The kind of the return value is the magnitude of __a__ with the sign of  __b__. That is, 

     -  If __b \>= 0__ then the result is __abs(a)__
     -  else if __b < 0__ it is -__abs(a)__.
     - if __b__ is _real_ and the processor distinguishes between __-0.0__ and __0.0__ then the
       result is __-abs(a)__

## __Examples__

Sample program:

```fortran
program demo_sign
implicit none
   print *,  sign( -12,  1 )
   print *,  sign( -12,  0 )
   print *,  sign( -12, -1 )

   print *,  sign( -12.0, [1.0, 0.0, -1.0] )

   print *,  'can I distinguise 0 from -0? ', sign( 1.0, -0.0 ) .ne. sign( 1.0, 0.0 )
end program demo_sign
```
Results:
```text
             12
             12
            -12
      12.00000       12.00000      -12.00000    
    can I distinguise 0 from -0?  F
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT)
# CSHIFT
## __Name__

__cshift__(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array


## __Syntax__
```fortran
result = cshift(array, shift, dim)
```
## __Description__

__cshift(array, shift \[, dim\])__ performs a circular shift on elements
of __array__ along the dimension of __dim__. If __dim__ is omitted it is taken to be
__1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= dim \<= n__,
where "n" is the rank of __array__. If the rank of __array__ is one, then all
elements of __array__ are shifted by __shift__ places. If rank is greater than
one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __shift__
    : The type shall be _integer_.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_cshift
implicit none
integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_cshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
    
     4  7  1
     8  2  5
     9  3  6
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# DOT_PRODUCT
## __Name__

__dot\_product__(3) - \[TRANSFORMATIONAL\] Dot product function


## __Syntax__
```fortran
result = dot_product(vector_a, vector_b)
```
## __Description__

__dot\_product(vector\_a, vector\_b)__ computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are _integer_ or _real_, the result is
__sum(vector\_a\*vector\_b)__. If the vectors are _complex_, the result is
__sum(conjg(vector\_a)\*vector\_b)__. If the vectors are _logical_, the
result is __any(vector\_a .and. vector\_b)__.

## __Arguments__

  - __vector\_a__
    : The type shall be numeric or _logical_, rank 1.

  - __vector\_b__
    : The type shall be numeric if vector\_a is of numeric type or _logical_
    if vector\_a is of type _logical_. vector\_b shall be a rank-one
    array.

## __Returns__

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is .true. or .false..

## __Examples__

Sample program:

```fortran
program demo_dot_prod
implicit none
    integer, dimension(3) :: a, b
    a = [ 1, 2, 3 ]
    b = [ 4, 5, 6 ]
    print '(3i3)', a
    print *
    print '(3i3)', b
    print *
    print *, dot_product(a,b)
end program demo_dot_prod
```
  Results:
```text
     1  2  3
   
     4  5  6
   
             32
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# EOSHIFT
## __Name__

__eoshift__(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array


## __Syntax__
```fortran
result = eoshift(array, shift, boundary, dim)
```
## __Description__

__eoshift(array, shift\[, boundary, dim\])__ performs an end-off shift
on elements of __array__ along the dimension of __dim__. If __dim__ is omitted it is
taken to be __1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= DIM
\<= n__ where __"n"__ is the rank of __array__. If the rank of __array__ is one, then
all elements of __array__ are shifted by __shift__ places. If rank is greater
than one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If __boundary__ is present then the corresponding value
of from __boundary__ is copied back in the other end. If __boundary__ is not
present then the following are copied in depending on the type of __array__.

\*Array Type\* - \*Boundary Value\*

   - Numeric 0 of the type and kind of __array__

   - Logical .false.

   - __Character(len)__ LEN blanks

## __Arguments__

  - __array__
    : May be any type, not scalar.

  - __shift__
    : The type shall be _integer_.

  - __boundary__
    : Same type as ARRAY.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_eoshift
implicit none
    integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_eoshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
   
     4  7 -5
     8 -5 -5
     6  9 -5
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# MATMUL
## __Name__

__matmul__(3) - \[TRANSFORMATIONAL\] matrix multiplication


## __Syntax__
```fortran
result = matmul(matrix_a, matrix_b)
```
## __Description__

Performs a matrix multiplication on numeric or logical arguments.

## __Arguments__

  - __matrix\_a__
    : An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
    one or two.

  - __matrix\_b__
    : An array of _integer_, _real_, or _complex_ type if __matrix\_a__ is of a
    numeric type; otherwise, an array of _logical_ type. The rank shall be
    one or two, and the first (or only) dimension of __matrix\_b__ shall be
    equal to the last (or only) dimension of __matrix\_a__.

## __Returns__

The matrix product of __matrix\_a__ and __matrix\_b__. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# PARITY
## __Name__

__parity__(3) - \[TRANSFORMATIONAL\] Reduction with exclusive __OR__()


## __Syntax__
```fortran
result = parity(mask, dim)

    function parity(mask, dim)
    type(logical(kind=LKIND))                    :: dim
    type(logical(kind=LKIND)),intent(in)         :: mask(..)
    type(integer(kind=KIND)),intent(in),optional :: dim
```
where KIND and LKIND are any supported kind for the type.
```
## __Description__

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

## __Arguments__

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

## __Returns__

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__ is
returned: __.true.__ if an odd number of elements are __.true.__ and __.false.__
otherwise.

When __dim__ is specified the returned shape is similar to that of __mask__
with dimension __dim__ dropped.

## __Examples__

Sample program:

```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x) 
end program demo_parity
```
  Results:
```text
    T
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# NULL
## __Name__

__null__(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer


## __Syntax__
```fortran
ptr => null(mold)

```
## __Description__

Returns a disassociated pointer.

If __mold__ is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In _Fortran 95_, __mold__ is optional. Please note that _Fortran 2003_ includes cases where it is required.

## __Arguments__

  - __mold__
    : (Optional) shall be a pointer of any association status and of any
    type.

## __Returns__

A disassociated pointer or an unallocated allocatable entity.

## __Examples__

Sample program:

```fortran
!program demo_null
module showit
implicit none
private
character(len=*),parameter :: g='(*(g0,1x))'
public gen
! a generic interface that only differs in the 
! type of the pointer the second argument is
interface gen
 module procedure s1
 module procedure s2
end interface

contains

subroutine s1 (j, pi)
 integer j
 integer, pointer :: pi
   if(associated(pi))then
      write(*,g)'Two integers in S1:,',j,'and',pi
   else
      write(*,g)'One integer in S1:,',j
   endif
end subroutine s1

subroutine s2 (k, pr)
 integer k
 real, pointer :: pr
   if(associated(pr))then
      write(*,g)'integer and real in S2:,',k,'and',pr
   else
      write(*,g)'One integer in S2:,',k
   endif
end subroutine s2

end module showit

use showit, only : gen

real,target :: x = 200.0
integer,target :: i = 100

real, pointer :: real_ptr
integer, pointer :: integer_ptr

! so how do we call S1() or S2() with a disassociated pointer?

! the answer is the null() function with a mold value

! since s1() and s2() both have a first integer
! argument the NULL() pointer must be associated
! to a real or integer type via the mold option
! so the following can distinguish whether s1(1)
! or s2() is called, even though the pointers are
! not associated or defined
 
call gen (1, null (real_ptr) )    ! invokes s2
call gen (2, null (integer_ptr) ) ! invokes s1
real_ptr => x
integer_ptr => i
call gen (3, real_ptr ) ! invokes s2
call gen (4, integer_ptr ) ! invokes s1

end
!end program demo_null
```
  Results:
```text
   One integer in S2:, 1
   One integer in S1:, 2
   integer and real in S2:, 3 and 200.000000
   Two integers in S1:, 4 and 100
```
## __Standard__

Fortran 95 and later

## __See Also__

[__associated__(3)](ASSOCIATED)

###### fortran-lang intrinsic descriptions
