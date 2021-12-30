---
layout: book
title: floor
permalink: /learn/intrinsics/FLOOR
---
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
