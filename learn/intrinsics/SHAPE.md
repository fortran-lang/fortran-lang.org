---
layout: book
title: shape
permalink: /learn/intrinsics/SHAPE
---
#### NAME

__shape__(3f) - \[ARRAY INQUIRY\] Determine the shape of an array
(GFDL)

#### SYNTAX

result = __shape__(source\[, kind\])

#### DESCRIPTION

Determines the shape of an array.

#### ARGUMENTS

  - __SOURCE__
    Shall be an array or scalar of any type. If SOURCE is a pointer it
    must be associated and allocatable arrays must be allocated.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

An INTEGER array of rank one with as many elements as SOURCE has
dimensions. The elements of the resulting array correspond to the extend
of SOURCE along the respective dimensions. If SOURCE is a scalar, the
result is the rank one array of size zero. If KIND is absent, the return
value has the default integer kind otherwise the specified kind.

#### EXAMPLE

Sample program:

```
   program demo_shape
   implicit none
     integer, dimension(-1:1, -1:2) :: a
     write(*,*) shape(a)             ! [ 3, 4 ]
     write(*,*) size(shape(42))      ! [ ]
   end program demo_shape
```

#### STANDARD

Fortran 95 and later; with KIND argument Fortran 2003 and later

#### CLASS

Inquiry function

#### SEE ALSO

__reshape__(3), __size__(3)
