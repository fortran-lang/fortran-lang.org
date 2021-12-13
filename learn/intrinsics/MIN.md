---
layout: book
title: min
permalink: /learn/intrinsics/MIN
---
## __Name__

__min__(3) - \[NUMERIC\] Minimum value of an argument list
(GFDL)

## __Syntax__
```fortran
result = min(a1, a2, a3, ... )
```
## __Description__

Returns the argument with the smallest (most negative) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2, a3, \`\`\`__
    : An expression of the same type and kind as __A1__.

## __Returns__

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

## __Examples__

Sample program

```fortran
program demo_min
implicit none
    write(*,*)min(10.0,11.0,30.0,-100.0)
end program demo_min
```

Results:

```
      -100.0000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__max__(3)](MAX),
[__minloc__(3)](MINLOC),
[__minval__(3)](MINVAL)

###### fortran-lang intrinsic descriptions
