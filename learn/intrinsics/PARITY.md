---
layout: book
title: parity
permalink: /learn/intrinsics/PARITY
---
### NAME

**parity**(3f) - \[TRANSFORMATIONAL FUNCTION\] Reduction with exclusive **OR**()
(GFDL)

### SYNTAX

result = **parity**(mask\[, dim\])

### DESCRIPTION

Calculates the parity (i.e. the reduction using .xor.) of MASK along
dimension DIM.

### ARGUMENTS

  - **MASK**
    Shall be an array of type LOGICAL.

  - **DIM**
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

### RETURN VALUE

The result is of the same type as MASK.

If DIM is absent, a scalar with the parity of all elements in MASK is
returned: .true. if an odd number of elements are .true. and

where "n" equals the rank of MASK, and a shape similar to that of MASK
with dimension DIM dropped is returned.

### EXAMPLE

Sample program:

```
   program demo_parity
   implicit none
     logical :: x(2) = [ .true., .false. ]
     print *, parity(x) ! T
   end program demo_parity
```

### STANDARD

Fortran 2008 and later

### CLASS

Transformational function
