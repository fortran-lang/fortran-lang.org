# MATH_index
### General mathematical functions

# ACOS
## __Name__
__acos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine (inverse cosine) function

## __Syntax__
```fortran
  result = acos(x)

   TYPE(kind=KIND),elemental :: acos

   TYPE(kind=KIND,intent(in) :: x
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type.

## __Description__

__acos(x)__ computes the arccosine of __x__ (inverse of __cos(x)__).

## __Arguments__

  - __x__
    : Must be type _real_ or _complex_. If the type is _real_, the value
    must satisfy |__x__| <= 1.

## __Returns__

The return value is of the same type and kind as __x__. The _real_ part of
the result is in radians and lies in the range __0 \<= acos(x%re) \<= PI__ .

## __Examples__
Sample program:
```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x = 0.866_real64
real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64

    print all,'acos(',x,') is ', acos(x)
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
    print all,'180 degrees is ', d2r*180.0_real64, ' radians'
    print all,'for reference &
    &PI ~ 3.14159265358979323846264338327950288419716939937510'
    print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])

end program demo_acos
```
  Results:
```text
   acos( .8660000000000000 ) is  .5236495809318289
   90 degrees is  1.570796326794897  radians
   180 degrees is  3.141592653589793  radians
   for reference PI ~ 3.14159265358979323846264338327950288419716939937510
   elemental 3.141593 2.094395 1.570796 1.047198 .000000
```
## __Standard__
FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [__cos__(3](COS))

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ACOSH
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

__acosh(x)__ computes the inverse hyperbolic cosine of __x__ in radians.

## __Arguments__

  - __x__
    : the type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __x__.

If __x__ is _complex_, the imaginary part of the result is in radians and
lies between

> __0 \<= aimag(acosh(x)) \<= PI__

## __Examples__

Sample program:

```fortran
program demo_acosh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]
   write (*,*) acosh(x)
end program demo_acosh
```
  Results:
```text
 0.000000000000000E+000   1.31695789692482        1.76274717403909
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__cosh__(3)](COSH)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ASIN
## __Name__

__asin__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function

## __Syntax__
```fortran
result = asin(x)

    elemental TYPE(kind=KIND) function asin(x)
    TYPE(kind=KIND) :: x
```
where the returned value has the kind of the input value 
and TYPE may be _real_ or _complex_

## __Description__

__asin(x)__ computes the arcsine of its argument __x__.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

## __Arguments__

  - __x__
    : The type shall be either _real_ and a magnitude that is less than or
    equal to one; or be _complex_.

## __Returns__

  - __result__
    : The return value is of the same type and kind as __x__. The real part of
    the result is in radians and lies in the range __-PI/2 \<=
    asin(x) \<= PI/2__.

## __Examples__

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

```fortran
program demo_asin
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to radians
real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp
real(kind=dp)           :: angle, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asin(rise/run)
  print all, 'angle of incline(radians) = ', angle
  angle = angle/D2R
  print all, 'angle of incline(degrees) = ', angle

  print all, 'percent grade=',rise/run*100.0_dp
end program demo_asin
```

Results:
```
    angle of incline(radians) =    2.5002604899361139E-002
    angle of incline(degrees) =    1.4325437375665075
    percent grade=   2.5000000000000000
```
The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run. In the example the rise is
1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.
Written as a percent this is 2.5 %.

For the US, two and 1/2 percent is generally thought of as the upper
limit. This means a rise of 2.5 feet when going 100 feet forward. In
the US this was the maximum grade on the first major US railroad, the
Baltimore and Ohio. Note curves increase the frictional drag on a
train reducing the allowable grade.

## __Standard__

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [__sin__(3)](SIN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ASINH
## __Name__

__asinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function


## __Syntax__
```fortran
result = asinh(x)

    elemental TYPE(kind=KIND) function asinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value 
and TYPE may be _real_ or _complex_

## __Description__

__asinh(x)__ computes the inverse hyperbolic sine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of the same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between 
__-PI/2 \<= aimag(asinh(x)) \<= PI/2__.

## __Examples__

Sample program:

```fortran
program demo_asinh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

    write (*,*) asinh(x)

end program demo_asinh
```
  Results:
```text
  -0.88137358701954305  0.0000000000000000  0.88137358701954305     
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__sinh__(3)](SINH)

###### fortran-lang intrinsic descriptions
# ATAN
## __Name__

__atan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

## __Syntax__
```fortran
  - result = __atan(y, x)__

   TYPE(kind=KIND):: atan
   TYPE(kind=KIND,intent(in) :: x
   TYPE(kind=KIND,intent(in),optional :: y
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type. If __y__ is present __x__ is _real`.

## __Description__

__atan(x)__ computes the arctangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_; if __y__ is present, __x__
      shall be _real_.

  - __y__
    : Shall be of the same type and kind as __x__. If __x__ is zero, __y__ 
    must not be zero.

## __Returns__

The returned value is of the same type and kind as __x__. If __y__ is
present, the result is identical to __atan2(y,x)__. Otherwise, it is the
arc tangent of __x__, where the real part of the result is in radians
and lies in the range
__-PI/2 \<= atan(x) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64),parameter :: &
 Deg_Per_Rad = 57.2957795130823208767981548_real64
real(kind=real64) :: x 
    x=2.866_real64
    print all, atan(x)

    print all, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad

end program demo_atan
```
  Results:
```text
   1.235085437457879
   .7853981633974483 45.00000000000000
   2.356194490192345 135.0000000000000
   -.7853981633974483 -45.00000000000000
   -2.356194490192345 -135.0000000000000
```
## __Standard__

FORTRAN 77 and later for a complex argument; and for two
arguments Fortran 2008 or later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

[__atan2__(3)](ATAN2), [__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ATAN2
## __Name__

__atan2__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

## __Syntax__
```fortran
result = atan2(y, x)
```
## __Description__

__atan2(y, x)__ computes the arctangent of the complex number
( __x__ + i __y__ ) .

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.
To convert from Cartesian Coordinates __(x,y)__ to polar coordinates

(r,theta): $$ \begin{aligned} r &= \sqrt{x**2 + y**2} \\ \theta
&= \tan**{__-1__}(y / x) \end{aligned} $$

## __Arguments__

  - __y__
    : The type shall be _real_.

  - __x__
    : The type and kind type parameter shall be the same as __y__. If __y__ is
    zero, then __x__ must be nonzero.

## __Returns__

The return value has the same type and kind type parameter as __y__. It is
the principal value of the complex number __(x + i, y)__. If x is nonzero,
then it lies in the range __-PI \<= atan(x) \<= PI__. The sign is
positive if __y__ is positive. If __y__ is zero, then the return value is zero
if __x__ is strictly positive, __PI__ if __x__ is negative and __y__ is positive zero
(or the processor does not handle signed zeros), and __-PI__ if __x__ is
negative and __Y__ is negative zero. Finally, if __x__ is zero, then the
magnitude of the result is __PI/2__.

## __Examples__

Sample program:

```fortran
program demo_atan2
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 1.e0_sp, y = 0.5e0_sp, z
   z = atan2(y,x)
   write(*,*)x,y,z
end program demo_atan2
```
Results:
```text
      1.00000000      0.500000000      0.463647604    
```

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# ATANH
## __Name__

__atanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function


## __Syntax__
```fortran
result = atanh(x)
```
## __Description__

__atanh(x)__ computes the inverse hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between

__-PI/2 \<= aimag(atanh(x)) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atanh
implicit none
real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]

   write (*,*) atanh(x)

end program demo_atanh
```
  Results:
```text
   -Infinity   0.00000000             Infinity
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__tanh__(3)](TANH)

###### fortran-lang intrinsic descriptions
# COS
## __Name__

__cos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

## __Syntax__
```fortran
result = cos(x)

   TYPE(kind=KIND),elemental :: cos
   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

## __Description__

__cos(x)__ computes the cosine of an angle __x__ given the size of the
angle in radians.

The cosine of a _real_ value is the ratio of the adjacent side to the
hypotenuse of a right-angled triangle.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.
    __x__ is assumed to be in radians.

## __Returns__

The return value is of the same type and kind as __x__. 

If __x__ is of the type _real_, the return value lies in
the range __-1 \<= cos(x) \<= 1__ .

## __Examples__

Sample program:

```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```
Results:
```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__
- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[__acos__(3)](ACOS),
[__sin__(3)](SIN),
[__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions
# COSH
## __Name__

__cosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function


## __Syntax__
```fortran
    result = cosh(x)

     TYPE(kind=KIND) elemental function cosh(x)
     TYPE(kind=KIND),intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any 
supported kind for the associated type. The returned __value__
will be the same type and kind as the input value __x__.

## __Description__

__cosh(x)__ computes the hyperbolic cosine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians.

If __x__ is _real_, the return value has a lower bound of one, 
__cosh(x) \>= 1__.

## __Examples__

Sample program:

```fortran
program demo_cosh
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = cosh(x)
end program demo_cosh
```

## __Standard__

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__acosh__(3)](ACOSH)

###### fortran-lang intrinsic descriptions
# SIN
## __Name__

__sin__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Sine function

## __Syntax__
```fortran
result = sin(x)

    elemental TYPE(kind=KIND) function sin(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

## __Description__

__sin(x)__ computes the sine of an angle given the size of the angle in
radians.

The sine of an angle in a right-angled triangle is the ratio of the
length of the side opposite the given angle divided by the length of the
hypotenuse.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_ in radians.

## __Returns__

  - __result__
    : The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program sample_sin
implicit none
real :: x = 0.0
   x = sin(x)
end program sample_sin
```

## __Haversine Formula__

From the article on "Haversine formula" in Wikipedia:

```text
The haversine formula is an equation important in navigation,
giving great-circle distances between two points on a sphere from
their longitudes and latitudes.
```

So to show the great-circle distance between the Nashville International
Airport (BNA) in TN, USA, and the Los Angeles International Airport
(LAX) in CA, USA you would start with their latitude and longitude,
commonly given as

```text
BNA: N 36 degrees 7.2',   W 86 degrees 40.2'
LAX: N 33 degrees 56.4',  W 118 degrees 24.0'
```

which converted to floating-point values in degrees is:

```text
     Latitude Longitude

   - BNA
     36.12, -86.67

   - LAX
     33.94, -118.40
```

And then use the haversine formula to roughly calculate the distance
along the surface of the Earth between the locations:

 Sample program:

```fortran
program demo_sin
implicit none
real :: d
    d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX
    print '(A,F9.4,A)', 'distance: ',d,' km'
contains
function haversine(latA,lonA,latB,lonB) result (dist)
!
! calculate great circle distance in kilometers
! given latitude and longitude in degrees
!
real,intent(in) :: latA,lonA,latB,lonB
real :: a,c,dist,delta_lat,delta_lon,lat1,lat2
real,parameter :: radius = 6371 ! mean earth radius in kilometers,
! recommended by the International Union of Geodesy and Geophysics

! generate constant pi/180
real, parameter :: deg_to_rad = atan(1.0)/45.0
   delta_lat = deg_to_rad*(latB-latA)
   delta_lon = deg_to_rad*(lonB-lonA)
   lat1 = deg_to_rad*(latA)
   lat2 = deg_to_rad*(latB)
   a = (sin(delta_lat/2))**2 + &
          & cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2
   c = 2*asin(sqrt(a))
   dist = radius*c
end function haversine
end program demo_sin
```
Results:

```text
    distance: 2886.4446 km
```

## __Standard__

FORTRAN 77 and later

## __See Also__
- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[__asin__(3)](ASIN),
[__cos__(3)](COS),
[__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# SINH
## __Name__

__sinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function

## __Syntax__
```fortran
result = sinh(x)

    elemental TYPE(kind=KIND) function sinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value 
and TYPE may be _real_ or _complex_

## __Description__

__sinh(x)__ computes the hyperbolic sine of __x__.

The hyperbolic sine of x is defined mathematically as:

   __sinh(x) = (exp(x) - exp(-x)) / 2.0__

If __x__ is of type _complex_ its imaginary part is regarded as a value
in radians.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_sinh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = - 1.0_real64
real(kind=real64) :: nan, inf
character(len=20) :: line

   print *, sinh(x)
   print *, (exp(x)-exp(-x))/2.0

   ! sinh(3) is elemental and can handle an array
   print *, sinh([x,2.0*x,x/3.0])

   ! a NaN input returns NaN
   line='NAN'
   read(line,*) nan
   print *, sinh(nan)

   ! a Inf input returns Inf
   line='Infinity'
   read(line,*) inf
   print *, sinh(inf)

   ! an overflow returns Inf
   x=huge(0.0d0)
   print *, sinh(x)

end program demo_sinh
```
Results:
```text
  -1.1752011936438014     
  -1.1752011936438014     
  -1.1752011936438014       -3.6268604078470190      -0.33954055725615012     
                       NaN
                  Infinity
                  Infinity
```
## __Standard__

Fortran 95 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__asinh__(3)](ASINH)

###### fortran-lang intrinsic descriptions
# TAN
## __Name__

__tan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function


## __Syntax__
```fortran
result = tan(x)
```
## __Description__

__tan(x)__ computes the tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_tan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 0.165_real64
     write(*,*)x, tan(x)
end program demo_tan
```
  Results:
```text
     0.16500000000000001       0.16651386310913616     
```
## __Standard__

FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.

## __See Also__

[__atan__(3)](ATAN),
[__cos__(3)](COS),
[__sin__(3)](SIN)

###### fortran-lang intrinsic descriptions
# TANH
## __Name__

__tanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function


## __Syntax__
```fortran
x = tanh(x)
```
## __Description__

__tanh(x)__ computes the hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is complex, the
imaginary part of the result is in radians. If __x__ is _real_, the return
value lies in the range

```
      -1 <= tanh(x) <= 1.
```
## __Examples__

Sample program:

```fortran
program demo_tanh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 2.1_real64
   write(*,*)x, tanh(x)
end program demo_tanh
```
  Results:
```text
      2.1000000000000001       0.97045193661345386     
```
## __Standard__

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__atanh__(3)](ATANH)

###### fortran-lang intrinsic descriptions
# RANDOM_NUMBER
## __Name__

__random\_number__(3) - \[MATHEMATICS:RANDOM\] Pseudo-random number


## __Syntax__
```fortran
   random_number(harvest)
```
## __Description__

Returns a single pseudorandom number or an array of pseudorandom numbers
from the uniform distribution over the range 0 \<= x \< 1.

## __Arguments__

  - __harvest__
    : Shall be a scalar or an array of type _real_.

## __Examples__

Sample program:

```fortran
program demo_random_number
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
integer, allocatable :: seed(:)
integer              :: n
integer              :: first,last
integer              :: i
integer              :: rand_int
integer,allocatable  :: count(:)
real(kind=dp)        :: rand_val
   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   first=1
   last=10
   allocate(count(last-first+1))
   ! To have a discrete uniform distribution on the integers 
   ! [first, first+1, ..., last-1, last] carve the continuous
   ! distribution up into last+1-first equal sized chunks, 
   ! mapping each chunk to an integer.
   !
   ! One way is:
   !   call random_number(rand_val)
   ! choose one from last-first+1 integers
   !   rand_int = first + FLOOR((last+1-first)*rand_val)
      count=0
      ! generate a lot of random integers from 1 to 10 and count them.
      ! with a large number of values you should get about the same
      ! number of each value
      do i=1,100000000
         call random_number(rand_val)
         rand_int=first+floor((last+1-first)*rand_val)
         if(rand_int.ge.first.and.rand_int.le.last)then
            count(rand_int)=count(rand_int)+1
         else
            write(*,*)rand_int,' is out of range'
         endif
      enddo
      write(*,'(i0,1x,i0)')(i,count(i),i=1,size(count))
end program demo_random_number
```
Results:
```
   1 10003588
   2 10000104
   3 10000169
   4 9997996
   5 9995349
   6 10001304
   7 10001909
   8 9999133
   9 10000252
   10 10000196
```
## __Standard__

Fortran 95 and later

## __See Also__

[__random\_seed__(3)](RANDOM_SEED)

###### fortran-lang intrinsic descriptions
# RANDOM_SEED
## __Name__

__random\_seed__(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence


## __Syntax__
```fortran
call random_seed(size, put, get)
```
## __Description__

Restarts or queries the state of the pseudorandom number generator used
by random\_number.

If random\_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

## __Arguments__

  - __size__
    : (Optional) Shall be a scalar and of type default _integer_, with
    __intent(out)__. It specifies the minimum size of the arrays used
    with the __put__ and __get__ arguments.

  - __put__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(in)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

  - __get__
    : (Optional) Shall be an array of type default _integer_ and rank one.
    It is __intent(out)__ and the size of the array must be larger than
    or equal to the number returned by the __size__ argument.

## __Examples__

Sample program:

```fortran
program demo_random_seed
implicit none
integer, allocatable :: seed(:)
integer :: n

   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   write (*, *) seed

end program demo_random_seed
```
  Results:
```text
     -674862499 -1750483360  -183136071  -317862567   682500039
     349459   344020729 -1725483289
```

## __Standard__

Fortran 95 and later

## __See Also__

[__random\_number__(3)](RANDOM_NUMBER)

###### fortran-lang intrinsic descriptions
# EXP
## __Name__

__exp__(3) - \[MATHEMATICS\] Exponential function

## __Syntax__
```fortran
result = exp(x)
```
## __Description__

__exp__(x) computes the base "_e_" exponential of __x__ where "_e_" is
_Euler's constant_.

If __x__ is of type _complex_, its imaginary part is regarded as a value
in radians such that (see _Euler's formula_):

if 
     __cx=(re,im)__ 
then 
     __exp(cx)=exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))__

Since __exp__(3) is the inverse function of __log__(3) the maximum valid magnitude
of the _real_ component of __x__ is __log(huge(x))__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The value of the result is __e\*\*x__ where __e__ is Euler's constant.

The return value has the same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_exp
implicit none
real :: x , re, im
complex :: cx

   x = 1.0
   write(*,*)"Euler's constant is approximately",exp(x)

   !! complex values
   ! given
   re=3.0
   im=4.0
   cx=cmplx(re,im)

   ! complex results from complex arguments are Related to Euler's formula
   write(*,*)'given the complex value ',cx
   write(*,*)'exp(x) is',exp(cx)
   write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

   ! exp(3) is the inverse function of log(3) so
   ! the real component of the input must be less than or equal to 
   write(*,*)'maximum real component',log(huge(0.0)) 
   ! or for double precision
   write(*,*)'maximum doubleprecision component',log(huge(0.0d0)) 

   ! but since the imaginary component is passed to the cos(3) and sin(3)
   ! functions the imaginary component can be any real value

end program demo_exp
```
Results:
```text
 Euler's constant is approximately   2.718282    
 given the complex value  (3.000000,4.000000)
 exp(x) is (-13.12878,-15.20078)
 is the same as (-13.12878,-15.20078)
 maximum real component   88.72284    
 maximum doubleprecision component   709.782712893384     
```
## __Standard__

FORTRAN 77 and later

## __See Also__

* [__log__(3)](LOG)

* Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

* Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# LOG
## __Name__

__log__(3) - \[MATHEMATICS\] Logarithm function


## __Syntax__
```fortran
result = log(x)
```
## __Description__

__log(x)__ computes the natural logarithm of __x__, i.e. the logarithm to
the base "e".

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as __x__. If __x__ is _complex_, the imaginary part OMEGA is in the range

__-PI__ \< OMEGA \<= PI.

## __Examples__

Sample program:

```fortran
program demo_log
implicit none
  real(kind(0.0d0)) :: x = 2.71828182845904518d0
  complex :: z = (1.0, 2.0)
  write(*,*)x, log(x)    ! will yield (approximately) 1
  write(*,*)z, log(z)
end program demo_log
```
  Results:
```text
      2.7182818284590451        1.0000000000000000     
   (1.00000000,2.00000000) (0.804718971,1.10714877)
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# LOG10
## __Name__

__log10__(3) - \[MATHEMATICS\] Base 10 logarithm function

## __Syntax__
```fortran
result = log10(x)
 
   real(kind=KIND) elemental function log10(x)
   real(kind=KIND),intent(in) :: x
```
## __Description__

__log10(x)__ computes the base 10 logarithm of __x__. This
is generally called the "common logarithm".

## __Arguments__

  - __x__
    : A _real_ value > 0 to take the log of.

## __Returns__

The return value is of type _real_ . The kind type parameter is
the same as __x__.

## __Examples__

Sample program:

```fortran
program demo_log10
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 10.0_real64

   x = log10(x)
   write(*,'(*(g0))')'log10(',x,') is ',log10(x)

   ! elemental
   write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &
                     & 100000.0, 1000000.0, 10000000.0])

end program demo_log10
```
  Results:
```text
   log10(1.0000000000000000) is 0.0000000000000000
      0.00000000       1.00000000       2.00000000       3.00000000  
      4.00000000       5.00000000       6.00000000       7.00000000    
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# SQRT
## __Name__

__sqrt__(3) - \[MATHEMATICS\] Square-root function

## __Syntax__
```fortran
result = sqrt(x)

   TYPE(kind=KIND) elemental function sqrt(x) result(value)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: value
```
Where TYPE may be _real_ or _complex_ and __KIND__ may be any
kind valid for the declared type.

## __Description__

__sqrt(x)__ computes the principal square root of __x__.

In mathematics, a square root of a number __x__ is a number __y__ such
that __y*y = x__.

The number whose square root is being considered is known as the
_radicand_.

Every nonnegative  number _x_ has two square roots of the same unique
magnitude, one positive and one negative. The nonnegative square root
is called the principal square root.

The principal square root of 9 is 3, for example, even though (-3)*(-3)
is also 9.

A _real_, _radicand_ must be positive.

Square roots of negative numbers are a special case of complex numbers,
where the components of the _radicand_ need not be positive in order to
have a valid square root.

## __Arguments__

  - __x__
    : If __x__ is real its value must be greater than or equal to zero.
    The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as __x__.

## __Examples__

Sample program:

```fortran
program demo_sqrt
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x, x2
complex :: z, z2

   x = 2.0_real64
   z = (1.0, 2.0)
   write(*,*)x,z

   x2 = sqrt(x)
   z2 = sqrt(z)
   write(*,*)x2,z2

   x2 = x**0.5
   z2 = z**0.5 
   write(*,*)x2,z2

end program demo_sqrt
```
  Results:
```text
  2.0000000000000000    (1.00000000,2.00000000)
  1.4142135623730951    (1.27201962,0.786151350)
  1.4142135623730951    (1.27201962,0.786151350)
```

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# HYPOT
## __Name__

__hypot__(3) - \[MATHEMATICS\] returns the distance between the point and the origin.

## __Syntax__
```fortran
result = hypot(x, y)

   real(kind=KIND) elemental function hypot(x,y) result(value)
   real(kind=KIND),intent(in) :: x, y
```
   where __x,y,value__ shall all be of the same __kind__.

## __Description__

__hypot(x,y)__ is referred to as the Euclidean distance function. It is equal to
__sqrt(x**2 + y**2)__, without undue underflow or overflow.

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.
 
__hypot(x,y)__ returns the distance between the point __<x,y>__ and the origin.

## __Arguments__

  - __x__
    : The type shall be _real_.

  - __y__
    : The type and kind type parameter shall be the same as __x__.

## __Returns__

The return value has the same type and kind type parameter as __x__.

The result is the positive magnitude of the distance of the point __<x,y>__ from the
origin __<0.0,0.0>__ .

## __Examples__

Sample program:

```fortran
program demo_hypot
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x, y 
real(kind=real32),allocatable :: xs(:), ys(:)
integer :: i
character(len=*),parameter :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

   x = 1.e0_real32
   y = 0.5e0_real32

   write(*,*)
   write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
   write(*,'(*(g0))')'units away from the origin'
   write(*,*)

   ! elemental
   xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
   ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

   write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
   write(*,f)"have distances from the origin of ",hypot(xs,ys)
   write(*,f)"the closest is",minval(hypot(xs,ys))

end program demo_hypot
```
Results:
```text
   point <1.00000000,0.500000000> is 1.11803401
   units away from the origin
   
   the points
      +1.00000000 +0.500000000
      +1.00000000 +0.250000000
      +10.0000000 -10.0000000
      +15.0000000 +0.250000000
      -1.00000000 -0.250000000
   have distances from the origin of 
      +1.11803401 +1.03077638
      +14.1421356 +15.0020828
      +1.03077638
   the closest is
      +1.03077638
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# BESSEL_J0
## __Name__

__bessel\_j0__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 0


## __Syntax__
```fortran
    result = bessel_j0(x)
```
## __Description__

__bessel\_j0(x)__ computes the Bessel function of the first kind
of order __0__ of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range 
__-0.4027 \<= bessel(0,x) \<= 1__. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besj0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.0_real64
   x = bessel_j0(x)
   write(*,*)x
end program demo_besj0
```
  Results:
```text
      1.0000000000000000     
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN), 
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1), 
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL_J1
## __Name__

__bessel\_j1__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 1


## __Syntax__
```fortran
    result = bessel_j1(x)
```
## __Description__

__bessel\_j1(x)__ computes the Bessel function of the first kind
of order __1__ of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range 
__-0.5818 \<= bessel(0,x) \<= 0.5818__ . It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besj1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
   x = bessel_j1(x)
   write(*,*)x
end program demo_besj1
```
  Results:
```text
     0.44005058574493350     
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_jn__(3)](BESSEL_JN), 
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1), 
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL_JN
## __Name__

__bessel\_jn__(3) - \[MATHEMATICS\] Bessel function of the first kind


## __Syntax__
```fortran
  result = bessel_jn(n, x)

  result = bessel_jn(n1, n2, x)
```
## __Description__

__bessel\_jn(n, x)__ computes the Bessel function of the first
kind of order __n__ of __x__. If __n__ and __x__ are arrays, their ranks and shapes
shall conform.

__bessel\_jn(n1, n2, x)__ returns an array with the Bessel function\|Bessel functions 
of the first kind of the orders __n1__ to __n2__.

## __Arguments__

  - __n__
    : Shall be a scalar or an array of type _integer_.

  - __n1__
    : Shall be a non-negative scalar of type _integer_.

  - __n2__
    : Shall be a non-negative scalar of type _integer_.

  - __x__
    : Shall be a scalar or an array of type _real_. For 
    __bessel\_jn(n1, n2, x)__ it shall be scalar.

## __Returns__

The return value is a scalar of type _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besjn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = bessel_jn(5,x)
    write(*,*)x
end program demo_besjn
```
  Results:
```text
      2.4975773021123450E-004
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1), 
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL_Y0
## __Name__

__bessel\_y0__(3) - \[MATHEMATICS\] Bessel function of the second kind of order 0


## __Syntax__
```fortran
    result = bessel_y0(x)
```
## __Description__

__bessel\_y0(x)__ computes the Bessel function of the second
kind of order 0 of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besy0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 0.0_real64
  x = bessel_y0(x)
  write(*,*)x
end program demo_besy0
```
  Results:
```text
                    -Infinity
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN), 
[__bessel\_y1__(3)](BESSEL_Y1), 
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL_Y1
## __Name__

__bessel\_y1__(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1


## __Syntax__
```fortran
    result = bessel_y1(x)
```
## __Description__

__bessel\_y1(x)__ computes the Bessel function of the second
kind of order 1 of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besy1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 1.0_real64
  write(*,*)x, bessel_y1(x)
end program demo_besy1
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN), 
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_yn__(3)](BESSEL_YN)

###### fortran-lang intrinsic descriptions
# BESSEL_YN
## __Name__

__bessel\_yn__(3) - \[MATHEMATICS\] Bessel function of the second kind


## __Syntax__
```fortran
  result = bessel_yn(n, x)

  result = bessel_yn(n1, n2, x)
```
## __Description__

__bessel\_yn(n, x)__ computes the Bessel function of the second
kind of order __n__ of __x__. If __n__ and __x__ are arrays, their ranks and shapes
shall conform.

__bessel\_yn(n1, n2, x)__ returns an array with the Bessel
function\|Bessel functions of the first kind of the orders __n1__ to __n2__.

## __Arguments__

  - __n__
    : Shall be a scalar or an array of type _integer_.

  - __n1__
    : Shall be a non-negative scalar of type _integer_.

  - __n2__
    : Shall be a non-negative scalar of type _integer_.

  - __x__
    : Shall be a scalar or an array of type _real_; for 
    __bessel\_yn(n1, n2, x)__ it shall be scalar.

## __Returns__

The return value is _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besyn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
  write(*,*) x,bessel_yn(5,x)
end program demo_besyn
```
  Results:
```text
      1.0000000000000000       -260.40586662581222     
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1)

###### fortran-lang intrinsic descriptions
# ERF
## __Name__

__erf__(3) - \[MATHEMATICS\] Error function


## __Syntax__
```fortran
result = erf(x)
```
## __Description__

__erf__(x) computes the error function of __x__, defined as 
$$
\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{__-t__^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_, of the same kind as __x__ and lies in the
range __-1__ \<= __erf__(x) \<= 1 .

## __Examples__

Sample program:

```fortran
program demo_erf
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erf(x)
end program demo_erf
```
  Results:
```text
     0.17000000000000001       0.18999246120180879     
```
## __Standard__

Fortran 2008 and later

## See also

[__erfc__(3)](ERFC)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

###### fortran-lang intrinsic descriptions
# ERFC
## __Name__

__erfc__(3) - \[MATHEMATICS\] Complementary error function


## __Syntax__
```fortran
result = erfc(x)

   elemental function erfc(x)
   real(kind=KIND) :: erfc
   real(kind=KIND),intent(in) :: x
```
## __Description__

__erfc__(x) computes the complementary error function of __x__.  Simpy put
this is equivalent to __1 - erf(x)__, but __erfc__ is provided because
of the extreme loss of relative accuracy if __erf(x)__ is called for
large __x__ and the result is subtracted from __1__.

__erfc(x)__ is defined as

<!--
$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt.
$$
-->

$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_x^{\infty} e^{-t^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as __x__. It lies in
the range

> 0 \<= __erfc__(x) \<= 2.

## __Examples__

Sample program:

```fortran
program demo_erfc
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erfc(x)
end program demo_erfc
```
  Results:
```text
     0.17000000000000001       0.81000753879819121     
```
## __Standard__

Fortran 2008 and later

## See also
[__erf__(3)](ERF)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

###### fortran-lang intrinsic descriptions license: MIT) @urbanjost
# ERFC_SCALED
## __Name__

__erfc\_scaled__(3) - \[MATHEMATICS\] Error function


## __Syntax__
```fortran
result = erfc_scaled(x)
```
## __Description__

__erfc\_scaled__(x) computes the exponentially-scaled complementary
error function of __x__:

$$
e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty}
e^{-t^2} dt.
$$

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and of the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_erfc_scaled
implicit none
real(kind(0.0d0)) :: x = 0.17d0
   x = erfc_scaled(x)
   print *, x 
end program demo_erfc_scaled
```
  Results:
```text
     0.83375830214998126     
```

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# GAMMA
## __Name__

__gamma__(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

## __Syntax__
```fortran
x = gamma(x)
```
## __Description__

__gamma(x)__ computes Gamma of __x__. For positive whole number values of __n__ the
Gamma function can be used to calculate factorials, as  __(n-1)!  == gamma(real(n))__.
That is 
```text
n! == gamma(real(n+1))
```

$$ \\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t $$

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as _x_.

## __Examples__

Sample program:

```fortran
program demo_gamma
use, intrinsic :: iso_fortran_env, only : wp=>real64
implicit none
real :: x, xa(4)
integer :: i

   x = gamma(1.0) 
   write(*,*)'gamma(1.0)=',x

   ! elemental
   xa=gamma([1.0,2.0,3.0,4.0])
   write(*,*)xa
   write(*,*)

   ! gamma(3) is related to the factorial function
   do i=1,20
      ! check value is not too big for default integer type
      if(factorial(i).gt.huge(0))then
         write(*,*)i,factorial(i)
      else
         write(*,*)i,factorial(i),int(factorial(i))
      endif
   enddo
   ! more factorials
   FAC: block
   integer,parameter :: n(*)=[0,1,5,11,170]
   integer :: j
      do j=1,size(n)
         write(*,'(*(g0,1x))')'factorial of', n(j),' is ', &
          & product([(real(i,kind=wp),i=1,n(j))]),  &
          & gamma(real(n(j)+1,kind=wp))
      enddo
   endblock FAC

contains

function factorial(i) result(f)
integer,parameter :: dp=kind(0d0)
integer,intent(in) :: i
real :: f
   if(i.le.0)then
      write(*,'(*(g0))')'<ERROR> gamma(3) function value ',i,' <= 0'
      stop      '<STOP> bad value in gamma function'
   endif
   f=gamma(real(i+1))
end function factorial

end program demo_gamma
```
  Results:
```text
    gamma(1.0)=   1.000000    
      1.000000       1.000000       2.000000       6.000000    
    
              1   1.000000               1
              2   2.000000               2
              3   6.000000               6
              4   24.00000              24
              5   120.0000             120
              6   720.0000             720
              7   5040.000            5040
              8   40320.00           40320
              9   362880.0          362880
             10   3628800.         3628800
             11  3.9916800E+07    39916800
             12  4.7900160E+08   479001600
             13  6.2270208E+09
             14  8.7178289E+10
             15  1.3076744E+12
             16  2.0922791E+13
             17  3.5568741E+14
             18  6.4023735E+15
             19  1.2164510E+17
             20  2.4329020E+18
   factorial of 0  is  1.000000000000000 1.000000000000000
   factorial of 1  is  1.000000000000000 1.000000000000000
   factorial of 5  is  120.0000000000000 120.0000000000000
   factorial of 11  is  39916800.00000000 39916800.00000000
   factorial of 170  is  .7257415615307994E+307 .7257415615307999E+307
```

## __Standard__

Fortran 2008 and later

## __See Also__

Logarithm of the Gamma function: [__log\_gamma__(3)](LOG_GAMMA)

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

###### fortran-lang intrinsic descriptions
# LOG_GAMMA
## __Name__

__log\_gamma__(3) - \[MATHEMATICS\] Logarithm of the Gamma function


## __Syntax__
```fortran
x = log_gamma(x)
```
## __Description__

__log\_gamma(x)__ computes the natural logarithm of the absolute value of the Gamma function.

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_log_gamma
implicit none
real :: x = 1.0
   write(*,*)x,log_gamma(x) ! returns 0.0
end program demo_log_gamma
```
  Results:
```text
      1.00000000       0.00000000    
```
## __Standard__

Fortran 2008 and later

## __See Also__

Gamma function: [__gamma__(3)](GAMMA)

###### fortran-lang intrinsic descriptions
# LOG_GAMMA
## __Name__

__log\_gamma__(3) - \[MATHEMATICS\] Logarithm of the Gamma function


## __Syntax__
```fortran
x = log_gamma(x)
```
## __Description__

__log\_gamma(x)__ computes the natural logarithm of the absolute value of the Gamma function.

## __Arguments__

  - __x__
    : Shall be of type _real_ and neither zero nor a negative integer.

## __Returns__

The return value is of type _real_ of the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_log_gamma
implicit none
real :: x = 1.0
   write(*,*)x,log_gamma(x) ! returns 0.0
end program demo_log_gamma
```
  Results:
```text
      1.00000000       0.00000000    
```
## __Standard__

Fortran 2008 and later

## __See Also__

Gamma function: [__gamma__(3)](GAMMA)

###### fortran-lang intrinsic descriptions
# NORM2
## __Name__

__norm2__(3) - \[MATHEMATICS\] Euclidean vector norm


## __Syntax__
```fortran
result = norm2(array, dim)

real function result norm2(array, dim)

   real,intent(in) :: array(..)
   integer,intent(in),optional :: dim
```
## __Description__

Calculates the Euclidean vector norm (L\_2 norm) of __array__ along
dimension __dim__.

## __Arguments__

  - __array__
    : Shall be an array of type _real_.

  - __dim__
    : shall be a scalar of type _integer_ with a value in the
    range from __1__ to  __rank(array)__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the square root of the sum of squares of
the elements of __array__ is returned. 

Otherwise, an array of rank __n-1__,
where __n__ equals the rank of __array__, and a shape similar to that of __array__
with dimension DIM dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_norm2
implicit none
integer :: i

real :: x(3,3) = reshape([ &
   1, 2, 3, &
   4, 5 ,6, &
   7, 8, 9  &
],shape(x),order=[2,1])

write(*,*) 'x='
write(*,'(4x,3f4.0)')transpose(x)

write(*,*) 'norm2(x)=',norm2(x)

write(*,*) 'x**2='
write(*,'(4x,3f4.0)')transpose(x**2)
write(*,*)'sqrt(sum(x**2))=',sqrt(sum(x**2))

end program demo_norm2
```
Results:
```text
 x=
      1.  2.  3.
      4.  5.  6.
      7.  8.  9.
 norm2(x)=   16.88194    
 x**2=
      1.  4.  9.
     16. 25. 36.
     49. 64. 81.
 sqrt(sum(x**2))=   16.88194    
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__product__(3)](PRODUCT),
[__sum__(3)](SUM),
[__hypot__(3)](HYPOT)

###### fortran-lang intrinsic descriptions
