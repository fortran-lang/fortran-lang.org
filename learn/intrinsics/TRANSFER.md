---
layout: book
title: transfer
permalink: /learn/intrinsics/TRANSFER
---
### NAME

__transfer__(3f) - \[BIT MANIPULATION\] Transfer bit patterns
(GFDL)

### SYNTAX

result = __transfer__(source, mold\[, size\])

### DESCRIPTION

Interprets the bitwise representation of SOURCE in memory as if it is
the representation of a variable or array of the same type and type
parameters as MOLD.

This is approximately equivalent to the C concept of \*casting\* one
type to another.

### ARGUMENTS

  - __SOURCE__
    Shall be a scalar or an array of any type.

  - __MOLD__
    Shall be a scalar or an array of any type.

  - __SIZE__
    (Optional) shall be a scalar of type INTEGER.

### RETURN VALUE

The result has the same type as MOLD, with the bit level representation
of SOURCE. If SIZE is present, the result is a one-dimensional array of
length SIZE. If SIZE is absent but MOLD is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of SOURCE.
If SIZE is absent and MOLD is a scalar, the result is a scalar.

If the bitwise representation of the result is longer than that of
SOURCE, then the leading bits of the result correspond to those of
SOURCE and any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as MOLD, the results are
undefined, and subsequent operations on the result cannot be guaranteed
to produce sensible behavior. For example, it is possible to create
LOGICAL variables for which VAR and .not. var both appear to be true.

### EXAMPLE

Sample program:

```
    program demo_transfer
    implicit none
      integer :: x = 2143289344
      print *, transfer(x, 1.0)    ! prints "nan" on i686
    end program demo_transfer
```

### COMMENTS

\[\[Joe Krahn\]\]: Fortran uses \*\*molding\*\* rather than
\*\*casting\*\*.

Casting, as in C, is an in-place reinterpretation. A cast is a device
that is built around an object to change its shape.

Fortran TRANSFER reinterprets data out-of-place. It can be considered
\*\*molding\*\* rather than casting. A \*\*mold\*\* is a device that
confers a shape onto an object placed into it.

The advantage of molding is that data is always valid in the context of
the variable that holds it. For many cases, a decent compiler should
optimize TRANSFER into a simple assignment.

There are disadvantages of this approach. It is problematic to define a
union of data types because you must know the largest data object, which
can vary by compiler or compile options. In many cases, an EQUIVALENCE
would be far more effective, but Fortran Standards committees seem
oblivious to the benefits of EQUIVALENCEs when used sparingly.

### STANDARD

Fortran 90 and later

### CLASS

Transformational function
