## anint
### __Name__

__anint__(3) - \[NUMERIC\] Nearest whole number


### __Syntax__
```fortran
result = anint(a, kind)
```
### __Description__

__anint(a \[, kind\])__ rounds its argument to the nearest whole number.

### __Arguments__

  - __a__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

### __Returns__

The return value is of type real with the kind type parameter of the
argument if the optional __kind__ is absent; otherwise, the kind type
parameter will be given by __kind__. If __a__ is greater than zero, __anint(a)__
returns __aint(a + 0.5)__. If __a__ is less than or equal to zero then it
returns __aint(a - 0.5)__.

### __Examples__

Sample program:

```fortran
program demo_anint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 1.234E0_real32
   x8 = 4.321_real64
   print *, anint(x4), dnint(x8)
   x8 = anint(x4,kind=real64)
   print *, x8
   print *
   ! elemental
   print *,anint([ &
    & -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0, &
    & +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_anint
```
  Results:
```text
    1.00000000       4.0000000000000000     
    1.0000000000000000     
  
   -3.00000000      -3.00000000      -2.00000000      -2.00000000
   -2.00000000      -1.00000000      -1.00000000       0.00000000
    1.00000000       1.00000000       2.00000000       2.00000000
    2.00000000       3.00000000       3.00000000
```
### __Standard__

FORTRAN 77 and later

### __See Also__

[__aint__(3)](AINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

####### fortran-lang intrinsic descriptions
