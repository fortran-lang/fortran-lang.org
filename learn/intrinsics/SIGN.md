---
layout: book
title: sign
permalink: /learn/intrinsics/SIGN
---
## __Name__

__sign__(3) - \[NUMERIC\] Sign copying function


## __Syntax__
```fortran
result = sign(a, b)

    elemental function sign(a, b)
    type(TYPE(kind=KIND))            :: sign
    type(TYPE(kind=KIND)),intent(in) :: a, b
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.
```
## __Description__

__sign__(a,b) returns the value of __a__ with the sign of __b__.


For processors that distinguish between positive and negative zeros
__sign()__ may be used to distinguish between __real__ values 0.0 and
−0.0. SIGN (1.0, -0.0) will return −1.0 when a negative zero is
distinguishable.

    29  1 Description. Magnitude of A with the sign of B.
      


## __Arguments__

  - __a__
    : Shall be of type _integer_ or _real_

  - __b__
    : Shall be of the same type and kind as __a__

## __Returns__

The kind of the return value is the magnitude of __a__ with the sign of
__b__. That is,

   - If __b \>= 0__ then the result is __abs(a)__
   - else if __b < 0__ it is -__abs(a)__.
   - if __b__ is _real_ and the processor distinguishes between __-0.0__
     and __0.0__ then the result is __-abs(a)__

## __Examples__

Sample program:

```fortran
program demo_sign
implicit none
   print *, sign( -12,  1 )
   print *, sign( -12,  0 )
   print *, sign( -12, -1 )

   print *, sign( -12.0, [1.0, 0.0, -1.0] )

   print *,'can I distinguish 0 from -0? ',sign(1.0,-0.0).ne.sign(1.0,0.0)
end program demo_sign
```
Results:
```text
             12
             12
            -12
      12.00000       12.00000      -12.00000    
    can I distinguish 0 from -0?  F
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
