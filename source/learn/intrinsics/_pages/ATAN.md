## atan
### __Name__

__atan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

### __Syntax__
```fortran
  - result = __atan(y, x)__

   TYPE(kind=KIND):: atan
   TYPE(kind=KIND,intent(in) :: x
   TYPE(kind=KIND,intent(in),optional :: y
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type. If __y__ is present __x__ is _real`.

### __Description__

__atan(x)__ computes the arctangent of __x__.

### __Arguments__

  - __x__
    : The type shall be _real_ or _complex_; if __y__ is present, __x__
      shall be _real_.

  - __y__
    : Shall be of the same type and kind as __x__. If __x__ is zero, __y__ 
    must not be zero.

### __Returns__

The returned value is of the same type and kind as __x__. If __y__ is
present, the result is identical to __atan2(y,x)__. Otherwise, it is the
arc tangent of __x__, where the real part of the result is in radians
and lies in the range
__-PI/2 \<= atan(x) \<= PI/2__

### __Examples__

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
### __Standard__

FORTRAN 77 and later for a complex argument; and for two
arguments Fortran 2008 or later

### __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

[__atan2__(3)](ATAN2), [__tan__(3)](TAN)

####### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
