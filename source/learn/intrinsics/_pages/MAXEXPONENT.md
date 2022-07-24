## maxexponent
### __Name__

__maxexponent__(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind


### __Syntax__
```fortran
result = maxexponent(x)
```
### __Description__

__maxexponent(x)__ returns the maximum exponent in the model of the type
of __x__.

### __Arguments__

  - __x__
    : Shall be of type _real_.

### __Returns__

The return value is of type _integer_ and of the default integer kind.

### __Examples__

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
### __Standard__

Fortran 95 and later

### __See Also__

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


####### fortran-lang intrinsic descriptions
