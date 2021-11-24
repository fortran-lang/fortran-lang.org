---
layout: book
title: unpack
permalink: /learn/intrinsics/UNPACK
---
### NAME

__unpack__(3f) - \[ARRAY CONSTRUCTION\] Store the elements of a vector in an array of higher rank
(GFDL)

### SYNTAX

result = __unpack__(vector, mask, field)

### DESCRIPTION

Store the elements of VECTOR in an array of higher rank.

### ARGUMENTS

  - __VECTOR__
    Shall be an array of any type and rank one. It shall have at least
    as many elements as MASK has TRUE values.

  - __MASK__
    Shall be an array of type LOGICAL.

  - __FIELD__
    Shall be of the same type as VECTOR and have the same shape as MASK.

### RETURN VALUE

The resulting array corresponds to FIELD with TRUE elements of MASK
replaced by values from VECTOR in array element order.

### EXAMPLE

Sample program:

```
    program demo_unpack
    implicit none
      integer :: vector(2)  = [1,1]
      logical :: mask(4)  = [ .true., .false., .false., .true. ]
      integer :: field(2,2) = 0, unity(2,2)

      ! result: unity matrix
      unity = unpack(vector, reshape(mask, [2,2]), field)
    end program demo_unpack
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

__pack__(3), __spread__(3)
