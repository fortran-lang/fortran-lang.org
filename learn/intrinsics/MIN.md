---
layout: book
title: min
permalink: /learn/intrinsics/MIN
---
### NAME

__min__(3f) - \[NUMERIC\] Minimum value of an argument list
(GFDL)

### SYNTAX

result = __min__(a1, a2 \[, a3, \`\`\`\])

### DESCRIPTION

Returns the argument with the smallest (most negative) value.

### ARGUMENTS

  - __A1__
    The type shall be INTEGER or REAL.

  - __A2, A3, \`\`\`__
    An expression of the same type and kind as A1.

### RETURN VALUE

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

### EXAMPLE

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

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__max__(3), __minloc__(3), __minval__(3)
