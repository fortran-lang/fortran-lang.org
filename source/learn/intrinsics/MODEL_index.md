# MODEL_index
### These routines support controlling and querying the current numeric model.

# EXPONENT
## __Name__

__exponent__(3) - \[MODEL\_COMPONENTS\] Exponent function


## __Syntax__
```fortran
result = exponent(x)
```
## __Description__

__exponent__(x) returns the value of the exponent part of __x__. If __x__ is
zero the value returned is zero.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type default _integer_.

## __Examples__

Sample program:

```fortran
program demo_exponent
implicit none
real :: x = 1.0
integer :: i
   i = exponent(x)
   print *, i
   print *, exponent(0.0)
end program demo_exponent
```
  Results:
```text
              1
              0
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# FRACTION
## __Name__

__fraction__(3) - \[MODEL\_COMPONENTS\] Fractional part of the model representation


## __Syntax__
```fortran
y = fraction(x)
```
## __Description__

__fraction(x)__ returns the fractional part of the model representation
of __x__.


## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

## __Returns__

The return value is of the same type and kind as the argument. The
fractional part of the model representation of __x__ is returned; it is 
__x \* radix(x)\*\*(-exponent(x))__.

## __Examples__

Sample program:

```fortran
program demo_fraction
implicit none
real :: x
   x = 178.1387e-4
   print *, fraction(x), x * radix(x)**(-exponent(x))
end program demo_fraction
```
  Results:
```text
     0.570043862      0.570043862    
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# NEAREST
## __Name__

__nearest__(3) - \[MODEL\_COMPONENTS\] Nearest representable number


## __Syntax__
```fortran
result = nearest(x, s)
```
## __Description__

__nearest(x, s)__ returns the processor-representable number nearest to
__x__ in the direction indicated by the sign of __s__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

  - __s__
    : Shall be of type _real_ and not equal to zero.

## __Returns__

The return value is of the same type as __x__. If __s__ is positive, __nearest__
returns the processor-representable number greater than __x__ and nearest to
it. If __s__ is negative, __nearest__ returns the processor-representable number
smaller than __x__ and nearest to it.

## __Examples__

Sample program:

```fortran
program demo_nearest
implicit none

   real :: x, y
   x = nearest(42.0, 1.0)
   y = nearest(42.0, -1.0)
   write (*,"(3(g20.15))") x, y, x - y

!  write (*,"(3(g20.15))") &
!   nearest(tiny(0.0),1.0), &
!   nearest(tiny(0.0),-1.0), &
!   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

!  write (*,"(3(g20.15))") &
!   nearest(huge(0.0),1.0), &
!   nearest(huge(0.0),-1.0), &
!   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

end program demo_nearest
```
  Results:
```text
   42.0000038146973    41.9999961853027    .762939453125000E-05
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# RRSPACING
## __Name__

__rrspacing__(3) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing


## __Syntax__
```fortran
result = rrspacing(x)
```
## __Description__

__rrspacing(x)__ returns the reciprocal of the relative spacing of model
numbers near __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of the same type and kind as __x__. The value returned
is equal to __abs(fraction(x)) \* float(radix(x))\*\*digits(x)__.

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# SCALE
## __Name__

__scale__(3) - \[MODEL\_COMPONENTS\] Scale a real value by a whole power of the radix


## __Syntax__
```fortran
result = scale(x, i)

   real(kind=KIND),intent(in) :: x
   integer,intent(in)         :: i
```
## __Description__

__scale(x,i)__ returns x \* __radix(x)\*\*i__.

## __Arguments__

  - __x__
    : The type of the argument shall be a _real_.

  - __i__
    : The type of the argument shall be a _integer_.

## __Returns__

The return value is of the same type and kind as __x__. Its value is 
__x \* radix(x)\*\*i__.

## __Examples__

Sample program:

```fortran
program demo_scale
implicit none
real :: x = 178.1387e-4
integer :: i = 5
   print *, scale(x,i), x*radix(x)**i
end program demo_scale
```

Results:

```
    0.570043862      0.570043862
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# SET_EXPONENT
## __Name__

__set\_exponent__(3) - \[MODEL\_COMPONENTS\] Set the exponent of the model


## __Syntax__
```fortran
result = set_exponent(x, i)
```
## __Description__

__set\_exponent(x, i)__ returns the real number whose fractional part is
that of __x__ and whose exponent part is __i__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is of the same type and kind as __x__. The real number
whose fractional part is that that of __x__ and whose exponent part if __i__ is
returned; it is __fraction(x) \* radix(x)\*\*i__.

## __Examples__

Sample program:

```fortran
program demo_setexp
implicit none
real :: x = 178.1387e-4
integer :: i = 17
   print *, set_exponent(x, i), fraction(x) * radix(x)**i
end program demo_setexp
```
  Results:
```text
      74716.7891       74716.7891    
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# SPACING
## __Name__

__spacing__(3) - \[MODEL\_COMPONENTS\] Smallest distance between two numbers of a given type


## __Syntax__
```fortran
result = spacing(x)
```
## __Description__

Determines the distance between the argument __x__ and the nearest adjacent
number of the same type.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The result is of the same type as the input argument __x__.

## __Examples__

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
   write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
end program demo_spacing
```
  Results:
```text
      1.19209290E-07
      2.2204460492503131E-016
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# DIGITS
## __Name__

__digits__(3) - \[NUMERIC MODEL\] Significant digits function

## __Syntax__
```fortran
result = digits(x)
    function digits(x)
    type(integer(kind=kind(0)))      :: digits
    type(TYPE(kind=KIND)),intent(in) :: x(..)
```
where TYPE may be _integer_ or _real_ and KIND is any kind supported by
TYPE.

## __Description__

__digits(x)__ returns the number of significant digits of the internal
model representation of __x__. For example, on a system using a 32-bit
floating point representation, a default real number would likely return
24.

## __Arguments__

  - __x__
    : The type may be a scalar or array of type _integer_ or _real_.

## __Returns__

The return value is of type _integer_ of default kind.

## __Examples__

Sample program:

```fortran
program demo_digits
implicit none
integer :: i = 12345
real :: x = 3.143
doubleprecision :: y = 2.33d0
   print *,'default integer:', digits(i)
   print *,'default real:   ', digits(x)
   print *,'default doubleprecision:', digits(y)
end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

## __Standard__

Fortran 95 and later

## __See Also__

[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# EPSILON
## __Name__

__epsilon__(3) - \[NUMERIC MODEL\] Epsilon function

## __Syntax__
```fortran
result = epsilon(x)
```
## __Description__

__epsilon(x)__ returns the floating point relative accuracy. 
It is the nearly negligible number relative to __1__
such that __1+ little_number__ is not equal to __1__; or more
precisely
```fortran
   real( 1.0, kind(x)) + epsilon(x) /=  real( 1.0, kind(x))
```
It may be thought of as the distance from 1.0 to the next largest
floating point number. 

One use of __epsilon__(3) is to select a _delta_ value for algorithms that
search until the calculation is within _delta_ of an estimate.

If _delta_ is too small the algorithm might never halt, as a computation
summing values smaller than the decimal resolution of the data type does
not change.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of the same type as the argument.

## __Examples__

Sample program:

```fortran
program demo_epsilon
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 3.143
real(kind=dp) :: y = 2.33d0

   ! so if x is of type real32, epsilon(x) has the value 2**-23
   print *, epsilon(x) 
   ! note just the type and kind of x matter, not the value
   print *, epsilon(huge(x)) 
   print *, epsilon(tiny(x)) 

   ! the value changes with the kind of the real value though
   print *, epsilon(y)

   ! adding and subtracting epsilon(x) changes x
   write(*,*)x == x + epsilon(x)
   write(*,*)x == x - epsilon(x)

   ! these next two comparisons will be .true. !
   write(*,*)x == x + epsilon(x) * 0.999999
   write(*,*)x == x - epsilon(x) * 0.999999

   ! you can calculate epsilon(1.0d0)
   write(*,*)my_dp_eps()

contains

function my_dp_eps()
! calculate the epsilon value of a machine the hard way
real(kind=dp) :: t
real(kind=dp) :: my_dp_eps

   ! starting with a value of 1, keep dividing the value
   ! by 2 until no change is detected. Note that with
   ! infinite precision this would be an infinite loop,
   ! but floating point values in Fortran have a defined
   ! and limited precision.
   my_dp_eps = 1.0d0
   SET_ST: do
      my_dp_eps = my_dp_eps/2.0d0
      t = 1.0d0 + my_dp_eps
      if (t <= 1.0d0) exit
   enddo SET_ST
   my_dp_eps = 2.0d0*my_dp_eps

end function my_dp_eps

end program demo_epsilon
```
  Results:
```text
  1.1920929E-07
  1.1920929E-07
  1.1920929E-07
  2.220446049250313E-016
 F
 F
 T
 T
  2.220446049250313E-016
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# HUGE
## __Name__

__huge__(3) - \[NUMERIC MODEL\] Largest number of a type and kind

## __Syntax__
```fortran
result = huge(x)

   function huge(x) result(answer)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: answer
```
   where __TYPE__ may be _real_ or _integer_ and __KIND__ is any supported
   associated _kind_.

## __Description__

__huge(x)__ returns the largest number that is not an infinity for the
kind and type of __x__.

## __Arguments__

  - __x__
    : Shall be an arbitrary value of type _real_ or _integer_.
    The value is used merely to determine what _kind_ and _type_ of
    scalar is being queried.

## __Returns__

The return value is of the same type and kind as _x_ and is the
largest value supported by the specified model.

## __Examples__

Sample program:

```fortran
program demo_huge
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer :: i,j,k,biggest
real :: v, w
   ! basic
   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)

   ! advanced
   biggest=huge(0)
   ! be careful of overflow when using integers in computation
   do i=1,14
      j=6**i   ! Danger, Danger
      w=6**i   ! Danger, Danger
      v=6.0**i
      k=v      ! Danger, Danger
      if(v.gt.biggest)then
         write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
      else
         write(*,f) i, j, k, v, v.eq.w
      endif
   enddo
end program demo_huge
```
Results:
```
  2147483647  3.4028235E+38  1.797693134862316E+308
  1.1754944E-38  2.225073858507201E-308

    1      6           6             6. T
    2      36          36            36. T
    3      216         216           216. T
    4      1296        1296          1296. T
    5      7776        7776          7776. T
    6      46656       46656         46656. T
    7      279936      279936        279936. T
    8      1679616     1679616       1679616. T
    9      10077696    10077696      10077696. T
    10     60466176    60466176      60466176. T
    11     362797056   362797056     362797056. T
    12    -2118184960 -2147483648    2176782336. F wrong for j and k and w
    13     175792128  -2147483648   13060694016. F wrong for j and k and w
    14     1054752768 -2147483648   78364164096. F wrong for j and k and w
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

# MAXEXPONENT
## __Name__

__maxexponent__(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind


## __Syntax__
```fortran
result = maxexponent(x)
```
## __Description__

__maxexponent(x)__ returns the maximum exponent in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_maxexponent
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x
real(kind=dp) :: y

   print *, minexponent(x), maxexponent(x)
   print *, minexponent(y), maxexponent(y)
end program demo_maxexponent
```
  Results:
```text
           -125         128
          -1021        1024
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# MINEXPONENT
## __Name__

__minexponent__(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind


## __Syntax__
```fortran
result = minexponent(x)
```
## __Description__

__minexponent(x)__ returns the minimum exponent in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_minexponent
use, intrinsic :: iso_fortran_env, only : &
 &real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x
real(kind=real64) :: y
    print *, minexponent(x), maxexponent(x)
    print *, minexponent(y), maxexponent(y)
end program demo_minexponent
```
Expected Results:
```
        -125         128
       -1021        1024
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# PRECISION
## __Name__

__precision__(3) - \[NUMERIC MODEL\] Decimal precision of a real kind


## __Syntax__
```fortran
result = precision(x)
```
## __Description__

__precision(x)__ returns the decimal precision in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_precision
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x(2)
complex(kind=dp) :: y

   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_precision
```
  Results:
```text
              6          37
             15         307
```
## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# RADIX
## __Name__

__radix__(3) - \[NUMERIC MODEL\] Base of a model number


## __Syntax__
```fortran
result = radix(x)
```
## __Description__

__radix(x)__ returns the base of the model representing the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _integer_ or _real_

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
   print *, "The radix for the doubleprecsion real kind is", radix(0.0d0)
end program demo_radix
```
  Results:
```text
    The radix for the default integer kind is           2
    The radix for the default real kind is           2
    The radix for the doubleprecsion real kind is           2
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


###### fortran-lang intrinsic descriptions
# RANGE
## __Name__

__range__(3) - \[NUMERIC MODEL\] Decimal exponent range of a real kind


## __Syntax__
```fortran
result = range(x)

      function range (x)
      integer :: range
      type(TYPE,kind=KIND),intent(in) :: x
```
   where TYPE is _real_ or _complex_ and KIND is any kind supported by 
   TYPE.
## __Description__

__range(x)__ returns the decimal exponent range in the model of the type
of __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_ or _complex_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_range
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp)    :: x(2)
complex(kind=dp) :: y
   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_range
```
  Results:
```text
              6          37
             15         307
```

## __Standard__

Fortran 95 and later

## __See Also__

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set\_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)

###### fortran-lang intrinsic descriptions
# TINY
## __Name__

__tiny__(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind


## __Syntax__
```fortran
result = tiny(x)
   real(kind=KIND) function(x)
   real(kind=KIND) :: x
```
  where KIND may be any kind supported by type _real_

## __Description__

__tiny(x)__ returns the smallest positive (non zero) number of the type
and kind of __x__.

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

[__digits__(3)](DIGITS),
[__epsilon__(3)](EPSILON),
[__exponent__(3)](EXPONENT),
[__fraction__(3)](FRACTION),
[__huge__(3)](HUGE),
[__maxexponent__(3)](MAXEXPONENT),
[__minexponent__(3)](MINEXPONENT),
[__nearest__(3)](NEAREST),
[__precision__(3)](PRECISION),
[__radix__(3)](RADIX),
[__range__(3)](RANGE),
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__set_exponent__(3)](SET_EXPONENT),
[__spacing__(3)](SPACING)

###### fortran-lang intrinsic descriptions
