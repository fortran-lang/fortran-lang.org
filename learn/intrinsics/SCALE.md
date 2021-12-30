---
layout: book
title: scale
permalink: /learn/intrinsics/SCALE
---
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
