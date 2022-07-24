## rrspacing
### __Name__

__rrspacing__(3) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing


### __Syntax__
```fortran
result = rrspacing(x)
```
### __Description__

__rrspacing(x)__ returns the reciprocal of the relative spacing of model
numbers near __x__.

### __Arguments__

  - __x__
    : Shall be of type _real_.

### __Returns__

The return value is of the same type and kind as __x__. The value returned
is equal to __abs(fraction(x)) \* float(radix(x))\*\*digits(x)__.

### __Standard__

Fortran 95 and later

### __See Also__

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


####### fortran-lang intrinsic descriptions
