---
layout: book
title: rank
permalink: /learn/intrinsics/RANK
---
### NAME

__rank__(3f) - \[ARRAY INQUIRY\] Rank of a data object
(GFDL)

### SYNTAX

result = __rank__(a)

### DESCRIPTION

__rank__(a) returns the rank of a scalar or array data object.

### ARGUMENTS

  - __A__
    can be of any type

### RETURN VALUE

The return value is of type INTEGER and of the default integer kind. For
arrays, their rank is returned; for scalars zero is returned.

### EXAMPLE

Sample program:

```
   program demo_rank
   implicit none
     integer :: a
     real, allocatable :: b(:,:)
     real  :: c(10,20,30)
     print *, rank(a), rank(b), rank(c)
   end program demo_rank
```

Results:

>   - __0__
>     2 3

Expected output:

### STANDARD

TS 29113

### CLASS

Inquiry function
