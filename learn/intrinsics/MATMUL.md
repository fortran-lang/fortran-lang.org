---
layout: book
title: matmul
permalink: /learn/intrinsics/MATMUL
---
### NAME

__matmul__(3f) - \[TRANSFORMATIONAL FUNCTION\] matrix multiplication
(GFDL)

### SYNTAX

result = __matmul__(matrix\_a, matrix\_b)

### DESCRIPTION

Performs a matrix multiplication on numeric or logical arguments.

### ARGUMENTS

  - __matrix\_a__
    An array of INTEGER, REAL, COMPLEX, or LOGICAL type, with a rank of
    one or two.

  - __matrix\_b__
    An array of INTEGER, REAL, or COMPLEX type if matrix\_a is of a
    numeric type; otherwise, an array of LOGICAL type. The rank shall be
    one or two, and the first (or only) dimension of matrix\_b shall be
    equal to the last (or only) dimension of matrix\_a.

### RETURN VALUE

The matrix product of matrix\_a and matrix\_b. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

### STANDARD

Fortran 95 and later

### CLASS

Transformational function
