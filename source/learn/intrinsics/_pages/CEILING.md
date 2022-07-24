## ceiling
### __Name__

__ceiling__(3) - \[NUMERIC\] Integer ceiling function


### __Syntax__
```fortran
result = ceiling(a, kind)

   integer(kind=KIND) elemental function ceiling(a,kind) 
   real(kind=ANY),intent(in)   :: a
   integer,intent(in),optional :: kind
```
### __Description__

__ceiling(a)__ returns the least integer greater than or equal to __a__.

### __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

### __Returns__

The return value is of type __integer__(kind) if __kind__ is present and a
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

### __Examples__

Sample program:

```fortran
program demo_ceiling
implicit none
real :: x = 63.29
real :: y = -63.59
   print *, ceiling(x) 
   print *, ceiling(y) 
   ! elemental
   print *,ceiling([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
end program demo_ceiling
```
  Results:
```text
   64
  -63
   -2      -2      -2      -2      -1      -1
    0       0       1       1       2       2
    3       3       3
```
### __Standard__

Fortran 95 and later

### __See Also__

[__floor__(3)](FLOOR),
[__nint__(3)](NINT)


[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND)

####### fortran-lang intrinsic descriptions
