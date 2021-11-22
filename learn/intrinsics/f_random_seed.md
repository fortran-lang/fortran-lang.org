---
layout: book
title: random_seed
permalink: /learn/intrinsics/f_random_seed
---
### NAME

**random\_seed**(3f) - \[MATHEMATICS:RANDOM\]
Initialize a pseudo-random number sequence

### SYNTAX

call **random\_seed**(\[size, put, get\])

### DESCRIPTION

Restarts or queries the state of the pseudorandom number generator used
by random\_number.

If random\_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

### ARGUMENTS

  - **SIZE**
    (Optional) Shall be a scalar and of type default INTEGER, with
    **intent**(out). It specifies the minimum size of the arrays used
    with the PUT and GET arguments.

  - **PUT**
    (Optional) Shall be an array of type default INTEGER and rank one.
    It is **intent**(in) and the size of the array must be larger than
    or equal to the number returned by the SIZE argument.

  - **GET**
    (Optional) Shall be an array of type default INTEGER and rank one.
    It is **intent**(out) and the size of the array must be larger than
    or equal to the number returned by the SIZE argument.

### EXAMPLE

Sample program:

```
   program demo_random_seed
     implicit none
     integer, allocatable :: seed(:)
     integer :: n

     call random_seed(size = n)
     allocate(seed(n))
     call random_seed(get=seed)
     write (*, *) seed
   end program demo_random_seed
```

### STANDARD

Fortran 95 and later

### CLASS

Subroutine

### SEE ALSO

**random\_number**(3)
