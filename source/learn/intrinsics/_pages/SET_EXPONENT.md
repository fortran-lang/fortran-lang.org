## set\_exponent
### __Name__

__set\_exponent__(3) - \[MODEL\_COMPONENTS\] Set the exponent of the model


### __Syntax__
```fortran
result = set_exponent(x, i)
```
### __Description__

__set\_exponent(x, i)__ returns the real number whose fractional part is
that of __x__ and whose exponent part is __i__.

### __Arguments__

  - __x__
    : Shall be of type _real_.

  - __i__
    : Shall be of type _integer_.

### __Returns__

The return value is of the same type and kind as __x__. The real number
whose fractional part is that that of __x__ and whose exponent part if __i__ is
returned; it is __fraction(x) \* radix(x)\*\*i__.

### __Examples__

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
[__rrspacing__(3)](RRSPACING),
[__scale__(3)](SCALE),
[__spacing__(3)](SPACING),
[__tiny__(3)](TINY)


####### fortran-lang intrinsic descriptions
