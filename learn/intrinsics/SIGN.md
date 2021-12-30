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
```
## __Description__

__sign__(a,b) returns the value of __a__ with the sign of __b__.

## __Arguments__

  - __a__
    : Shall be of type _integer_ or _real_

  - __b__
    : Shall be of the same type and kind as A

## __Returns__

The kind of the return value is that of __a__ and __b__. If __b \>= 0__ then the
result is __abs(a)__, else it is -__abs(a)__.

## __Examples__

Sample program:

```fortran
program demo_sign
implicit none
   print *, sign(-12,1)
   print *, sign(-12,0)
   print *, sign(-12,-1)

   print *, sign(-12.,1.)
   print *, sign(-12.,0.)
   print *, sign(-12.,-1.)
end program demo_sign
```
  Results:
```text
             12
             12
            -12
      12.0000000    
      12.0000000    
     -12.0000000    
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
