---
layout: book
title: min
permalink: /learn/intrinsics/MIN
---
## __Name__

__min__(3) - \[NUMERIC\] Minimum value of an argument list
(GFDL)

## __Syntax__

result = __min__(a1, a2 \[, a3, \`\`\`\])

## __Description__

Returns the argument with the smallest (most negative) value.

## __Arguments__

  - __A1__
    The type shall be _integer_ or _real_.

  - __A2, A3, \`\`\`__
    An expression of the same type and kind as A1.

## __Returns__

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

## __Examples__

Sample program

```
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

__max__(3), __minloc__(3), __minval__(3)
