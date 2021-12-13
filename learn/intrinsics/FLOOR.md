---
layout: book
title: floor
permalink: /learn/intrinsics/FLOOR
---
## __Name__

__floor__(3) - \[NUMERIC\] Integer floor function
(GFDL)

## __Syntax__
```fortran
result = floor(a, kind)
```
## __Description__

__floor__(a) returns the greatest integer less than or equal to __a__.

## __Arguments__

  - __a__
    : The type shall be _real_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer(kind)_ if __kind__ is present and of
default-kind _integer_ otherwise.

## __Examples__

Sample program:

```fortran
program demo_floor
implicit none
real :: x = 63.29
real :: y = -63.59
    print *, floor(x) 
    print *, floor(y) 
end program demo_floor
```
  Results:
```text
             63
            -64
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ceiling__(3)](CEILING),
[__nint__(3)](NINT)

###### fortran-lang intrinsic descriptions
