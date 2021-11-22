---
layout: book
title: min
permalink: /learn/intrinsics/f_min
---
### NAME

**min**(3f) - \[NUMERIC\] Minimum value of an argument
list

### SYNTAX

result = **min**(a1, a2 \[, a3, \`\`\`\])

### DESCRIPTION

Returns the argument with the smallest (most negative) value.

### ARGUMENTS

  - **A1**
    The type shall be INTEGER or REAL.

  - **A2, A3, \`\`\`**
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

Elemental procedure|Elemental function

### SEE ALSO

**max**(3), **minloc**(3), **minval**(3)
