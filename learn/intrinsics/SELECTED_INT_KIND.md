---
layout: book
title: selected_int_kind
permalink: /learn/intrinsics/SELECTED_INT_KIND
---
### NAME

**selected\_int\_kind**(3f) - \[KIND\] Choose integer kind
(GFDL)

### SYNTAX

result = **selected\_int\_kind**(r)

### DESCRIPTION

**selected\_int\_kind**(r) return the kind value of the smallest integer
type that can represent all values ranging from **-10**\*\*R (exclusive)
to 10\*\*R (exclusive). If there is no integer kind that accommodates
this range, selected\_int\_kind returns **-1**.

### ARGUMENTS

  - **R**
    Shall be a scalar and of type INTEGER.

### EXAMPLE

Sample program:

```
   program demo_selected_int_kind
   implicit none
     integer,parameter :: k5 = selected_int_kind(5)
     integer,parameter :: k15 = selected_int_kind(15)
     integer(kind=k5) :: i5
     integer(kind=k15) :: i15

     print *, huge(i5), huge(i15)

     ! the following inequalities are always true
     print *, huge(i5) >= 10_k5**5-1
     print *, huge(i15) >= 10_k15**15-1
   end program demo_selected_int_kind
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function
